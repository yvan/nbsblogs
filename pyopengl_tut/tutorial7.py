'''
http://pyopengl.sourceforge.net/context/tutorials/shader_7.html

you can use a special structure to bind together material properties in a
structure here the struct called Material

we can store things in arrays in shaders for cleaner code. this script is
basically doing the same thing as tutorial 6 but with more lights
'''

from OpenGLContext import testingcontext
BaseContext = testingcontext.getInteractive()
from OpenGL.GL import *
from OpenGL.arrays import vbo
from OpenGLContext.arrays import *
from OpenGL.GL import shaders
from OpenGLContext.scenegraph.basenodes import Sphere

class TestContext(BaseContext):
    def OnInit(self):
        # new structure that contains
        # material properties
        materialStruct = '''
        struct Material {
            vec4 ambient;
            vec4 diffuse;
            vec4 specular;
            float shininess;
        };
        '''

        phong_weightCalc = '''
        vec2 phong_weightCalc(
            in vec3 light_pos,
            in vec3 half_light,
            in vec3 frag_normal,
            in float shininess
        ){
            float n_dot_pos = max(0.0, dot(frag_normal, light_pos));
            float n_dot_half = 0.0;
            // changed from tutorial 6 phong weight calc after my inblog
            // investigation
            if (n_dot_pos >= 0.0){
                n_dot_half = pow(max(0.0, dot(half_light, frag_normal)), shininess);
            }
            return vec2(n_dot_pos, n_dot_half);
        }
        '''

        vs = '''
        attribute vec3 Vertex_position;
        attribute vec3 Vertex_normal;
        varying vec3 baseNormal;
        void main(){
            gl_Position = gl_ModelViewProjectionMatrix * vec4(Vertex_position, 1.0);
            baseNormal = gl_NormalMatrix * normalize(Vertex_normal);
        }
        '''
        vertex = shaders.compileShader(vs, GL_VERTEX_SHADER)

        fs = '''
        // define 3 lights containing
        // ambient, diffuse, and specular, and position (4 things)
        // 4(vectors per light)*3(lights) = 12
        uniform vec4 lights [12];
        uniform Material material;
        uniform vec4 Global_ambient;
        varying vec3 baseNormal;
        void main(){
            // setup the baseline color for the fragment
            vec4 fragColor = Global_ambient * material.ambient;

            // setup some constant indices for referencing parts
            // of lights
            int AMBIENT = 0;
            int DIFFUSE = 1;
            int SPECULAR = 2;
            int POSITION = 3;

            // for every light
            int i;
            for (i=0; i<12;i=i+4){
                //normalize light location, eye coordinates
                vec3 EC_Light_location = normalize(gl_NormalMatrix * lights[i+POSITION].xyz);

                // half light vector calculation
                vec3 Light_half = normalize(EC_Light_location - vec3(0,0,-1));

                // get phong weights
                vec2 weights = phong_weightCalc(
                    EC_Light_location,
                    Light_half,
                    baseNormal,
                    material.shininess
                );

                // calculate/update the fragment color with new information
                fragColor = (
                    fragColor
                    + (lights[i+AMBIENT] * material.ambient)
                    + (lights[i+DIFFUSE] * material.diffuse * weights.x)
                    + (lights[i+SPECULAR] * material.specular * weights.y)
                );
            }
            gl_FragColor = fragColor;
        }
        '''
        fragment = shaders.compileShader(phong_weightCalc + materialStruct + fs, GL_FRAGMENT_SHADER)
        self.shader = shaders.compileProgram(vertex, fragment)
        # create two vbos to represent a sphere, coordinates and indicies
        # and return a count to tell us how many things to display
        self.coords,self.indices, self.count = Sphere(radius=1).compile()

        # get uniform variable locations ready to be populated
        self.UNIFORM_VALUES = [
        ('Global_ambient',(.05,.05,.05,1.0)),
        ('material.ambient',(.2,.2,.2,1.0)),
        ('material.diffuse',(.5,.5,.5,1.0)),
        ('material.specular',(.8,.8,.8,1.0)),
        ('material.shininess',(.995,)),
        ]

        self.uniform_locations = {}
        for uniform, _ in self.UNIFORM_VALUES:
            location = glGetUniformLocation(self.shader, uniform)
            if location in (None, -1):
                print('Warning, no uniform: {}'.format(uniform))
            self.uniform_locations[uniform] = location
        self.uniform_locations['lights'] = glGetUniformLocation(self.shader, 'lights')

        # get attribute values ready to be populated
        attribute_vals = (
        'Vertex_position',
        'Vertex_normal')

        for attribute in attribute_vals:
            location = glGetAttribLocation(self.shader, attribute)
            if location in (None, -1):
                print('Warning no attribute: {}'.format(attribute))
            setattr(self, attribute+'_loc', location)

        # define our actual lights, ambient, diffuse, specular
        # contributions as well as position
        self.LIGHTS = array([
               [(.05,.05,.05,1.0),#'lights[0].ambient'
                (.3,.3,.3,1.0),#'lights[0].diffuse'
                (1.0,0.0,0.0,1.0),#'lights[0].specular'
                (4.0,2.0,10.0,0.0),#'lights[0].position
                (.05,.05,.05,1.0),#'lights[1].ambient'
                (.3,.3,.3,1.0),#'lights[1].diffuse'
                (0.0,1.0,0.0,1.0),#'lights[1].specular'
                (-4.0,2.0,10.0,0.0),# 'lights[1].position
                (.05,.05,.05,1.0),#('lights[2].ambient',
                (.3,.3,.3,1.0),#lights[2].diffuse'
                (0.0,0.0,1.0,1.0),#'lights[2].specular'
                (-4.0,2.0,-10.0,0.0)#'lights[2].position
                ]
        ], 'f')

    def Render(self, mode=None):
        BaseContext.Render(self, mode)
        glUseProgram(self.shader)
        try:
            # bind in our coordinates and indices
            # into gpu ram
            self.coords.bind()
            self.indices.bind()
            stride = self.coords.data[0].nbytes
            try:
                # set our lights with the values defined above
                # this is a different function '4fv', instead of '4f'
                glUniform4fv(self.uniform_locations['lights'],
                12, self.LIGHTS)

                for uniform, value in self.UNIFORM_VALUES:
                    location = self.uniform_locations.get(uniform)
                    if location not in (None, -1):
                        if len(value) == 4:
                            glUniform4f(location, *value)
                        elif len(value) == 3:
                            glUniform3f(location, *value)
                        elif len(value) == 1:
                            glUniform1f(location, *value)

                # setup all our vertices / normals and draw them
                glEnableVertexAttribArray(self.Vertex_position_loc)
                glEnableVertexAttribArray(self.Vertex_normal_loc)

                glVertexAttribPointer(self.Vertex_position_loc,
                3, GL_FLOAT, False, stride, self.coords)
                glVertexAttribPointer(self.Vertex_normal_loc,
                3, GL_FLOAT, False, stride, self.coords+(5*4))

                glDrawElements(GL_TRIANGLES, self.count, GL_UNSIGNED_SHORT, self.indices)
            finally:
                self.coords.unbind()
                self.indices.unbind()
                glDisableVertexAttribArray(self.Vertex_position_loc)
                glDisableVertexAttribArray(self.Vertex_normal_loc)
        finally:
            glUseProgram(0)

if __name__ == '__main__':
    TestContext.ContextMainLoop()

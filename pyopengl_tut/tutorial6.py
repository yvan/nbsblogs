'''
http://pyopengl.sourceforge.net/context/tutorials/shader_6.html

specular highlights indexed geometry, directional lighting this tutorial focuses
on lighting but theres a few other concepts as well.

indexed rendering - instead of using a manually created set of coordinates for
triangles we can use a pre built structure like the sphere which contains two
vbos one with vetex data and one with indices the render order is then read from
the indicies that way we can reuse a vertex by adding its index again and save
space on the gpu.

phong/blinn reflectance /shininess - not sure i totally understand whats going
on here or why we simplified the equation. ok i think i get it. this wikipedia
page clarified things somewhat:
https://en.wikipedia.org/wiki/Blinn%E2%80%93Phong_shading_model

basically instead of repeatedly calculating the angle between R and V (the angle
we are currently viewing to the reflected specular light) we calculate a
precomputed vector in between the light source vector and the vector of the
viewer of the light.

another thing the main graphic on the wikipedia page solves is understanding how
to think about the vectors. just think of them all pointing out of the surface,
the explanation on mathisfun is really good basically we want the component of
the normal, the thing that represents teh way this fragment is facing, that lies
along the light ray. if the result of dot prodct is 0 the angle is 90. and we
return 0 below. if the angle is greater than 90 we will get a negative dot
product. below we accept small negative dot products up to -0.05. apparently
this helps us get softer light? i mean in theory this helps us get softer light.
the thing is if we have any negative dot product beyond that the function
returns 0.
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
        phong_weightCalc = '''
        vec2 phong_weightCalc(
            in vec3 light_pos,
            in vec3 half_light,
            in vec3 frag_normal,
            in float shininess
        ){
            // get the dot product of the normal and the light ray
            float n_dot_pos = max(0.0, dot(frag_normal, light_pos));

            // set the specular reflection to 0
            float n_dot_half = 0.0;
            if (n_dot_pos >= 0.0){
                // get the transformed dot product of the 'half light'
                // this is our new speccular reflection weight
                n_dot_half = pow(max(0.0, dot(half_light, frag_normal)), shininess);
            }
            // return both the diffuse and specular reflection, n_dot_pos is the
            // weight of the diffuse light, n_dot_half is the weight for the
            // specular light
            return vec2(n_dot_pos, n_dot_half);
        }
        '''

        vs = '''
        attribute vec3 Vertex_position;
        attribute vec3 Vertex_normal;
        varying vec3 baseNormal;
        void main(){
            // store the vertex position
            gl_Position = gl_ModelViewProjectionMatrix * vec4(Vertex_position, 1.0);
            // store the vertex normal
            baseNormal = gl_NormalMatrix * normalize(Vertex_normal);
        }
        '''
        vertex = shaders.compileShader(vs, GL_VERTEX_SHADER)

        fs = '''
        // define a bunch of stuff
        uniform vec4 Global_ambient;
        uniform vec4 Light_ambient;
        uniform vec4 Light_diffuse;
        uniform vec4 Light_specular;
        uniform vec3 Light_location;
        uniform float Material_shininess;
        uniform vec4 Material_specular;
        uniform vec4 Material_ambient;
        uniform vec4 Material_diffuse;
        varying vec3 baseNormal;
        void main(){
            // get the light location in eye space
            vec3 EC_Light_location = normalize(gl_NormalMatrix * Light_location);

            // get the half light between the light ray and the viewpoint
            vec3 Light_half = normalize(EC_Light_location - vec3(0,0,-1));

            //get the weights for specular and diffuse light from the current view
            vec2 weights = phong_weightCalc(
                EC_Light_location,
                Light_half,
                baseNormal,
                Material_shininess
            );

            // return the fragment color with various lighting added in
            gl_FragColor = clamp(
                // global ambient light
                (Global_ambient * Material_ambient)
                // ambient light from light source
                + (Light_ambient * Material_ambient)
                // the diffuse light calcualted by blinn-phong function
                + (Light_diffuse * Material_diffuse * weights.x)
                // the specular light (spotlight) calculated by blinn-phong function
                + (Light_specular * Material_specular * weights.y)
                , 0.0, 1.0);
        }
        '''
        fragment = shaders.compileShader(phong_weightCalc + fs, GL_FRAGMENT_SHADER)
        self.shader = shaders.compileProgram(vertex, fragment)
        # create two vbos to represent a sphere
        self.coords, self.indices, self.count = Sphere(radius=1).compile()

        # setup uniform values,
        # basically map each memory
        # location in our script with
        # an attribute on our python class
        uniform_vals = (
        'Global_ambient',
        'Light_ambient',
        'Light_diffuse',
        'Light_location',
        'Light_specular',
        'Material_ambient',
        'Material_diffuse',
        'Material_shininess',
        'Material_specular')

        for uniform in uniform_vals:
            location = glGetUniformLocation(self.shader, uniform)
            if location in (None, -1):
                print('Warning, no uniform: {}'.format(uniform))
            setattr(self, uniform+'_loc', location)

        # do what we did before for shader attribute values
        attribute_vals = (
        'Vertex_position',
        'Vertex_normal')

        for attribute in attribute_vals:
            location = glGetAttribLocation(self.shader, attribute)
            if location in (None, -1):
                print('Warning no attribute: {}'.format(attribute))
            setattr(self, attribute+'_loc', location)

    def Render(self, mode=None):
        BaseContext.Render(self, mode)
        glUseProgram(self.shader)
        try:
            self.coords.bind()
            self.indices.bind()
            # set our stride value to be the number of bytes per coordinate
            stride = self.coords.data[0].nbytes
            try:
                # set our uniform values to their values
                glUniform4f( self.Global_ambient_loc, .05,.05,.05,.1 )
                glUniform4f( self.Light_ambient_loc, .1,.1,.1, 1.0 )
                glUniform4f( self.Light_diffuse_loc, .25,.25,.25,1 )
                glUniform4f( self.Light_specular_loc, 0.0,1.0,0,1 )
                glUniform3f( self.Light_location_loc, 6,2,4 )
                glUniform4f( self.Material_ambient_loc, .1,.1,.1, 1.0 )
                glUniform4f( self.Material_diffuse_loc, .15,.15,.15, 1 )
                glUniform4f( self.Material_specular_loc, 1.0,1.0,1.0, 1.0 )
                glUniform1f( self.Material_shininess_loc, .95)

                # enable and draw all our data points
                glEnableVertexAttribArray(self.Vertex_position_loc)
                glEnableVertexAttribArray(self.Vertex_normal_loc)
                glVertexAttribPointer(self.Vertex_position_loc,
                3, GL_FLOAT, False, stride, self.coords)
                glVertexAttribPointer(self.Vertex_normal_loc,
                3, GL_FLOAT, False, stride, self.coords+(5*4))
                # draw 'self.count' number of things
                # (this was stored when we made the sphere)
                # notice that glDrawElements is different
                # from glDrawArrays, this is the indexed
                # vbo scheme where we reuse verts and render
                # based on indices
                glDrawElements(
                    GL_TRIANGLES, self.count,
                    GL_UNSIGNED_SHORT, self.indices
                )
            finally:
                self.coords.unbind()
                self.indices.unbind()
                glDisableVertexAttribArray(self.Vertex_position_loc)
                glDisableVertexAttribArray(self.Vertex_normal_loc)
        finally:
            glUseProgram(0)

if __name__ == '__main__':
    TestContext.ContextMainLoop()

'''
specular highlights indexed geometry, directional lighting

http://pyopengl.sourceforge.net/context/tutorials/shader_6.html

this tutorial focuses on lighting but theres a few other concepts as well.



indexed rendering - instead of using a manually created set of cooords for triangles
we can use a pre built structure like the sphere which contains two vbos one with
vetex data and one with indices the render order is then read from the indicies
that way we can reuse a vertex by addin gits index again and save space on the gpu.

fragment shading - applying a different lighting value in the fragment shader
to make things look better

phong/blinn reflectance /shininess - not sure i totally understand whats going on here or
why we simplified the equation.

we actually used blinn-phong shading: https://en.wikipedia.org/wiki/Blinn-Phong_shading_model


i'm not sure the ambient and diffuse shaders are actually CONTRIBUTING anything to the
visuals here as when i take them out nothing changes, and when i take out the specular light
the whole sphere disappears? so seems like they do nothing here.
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
            // get the dot product of the noraml and the light ray
            float n_dot_pos = max(0.0, dot(frag_normal, light_pos));
            float n_dot_half = 0.0;
            if (n_dot_pos > -0.05){
                // get the transformed dot product of the 'half light'
                n_dot_half = pow(max(0.0, dot(half_light, frag_normal)), shininess);
                // return both the light and half light
                return vec2(n_dot_pos, n_dot_half);
            }
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
            // get the half light
            vec3 Light_half = normalize(EC_Light_location - vec3(0,0,-1));
            vec2 weights = phong_weightCalc(
                EC_Light_location,
                Light_half,
                baseNormal,
                Material_shininess
            );
            // return the fragment color
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
        self.coords,self.indices, self.count = Sphere(radius=1).compile()

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
            stride = self.coords.data[0].nbytes
            try:
                glUniform4f( self.Global_ambient_loc, .05,.05,.05,.1 )
                glUniform4f( self.Light_ambient_loc, .1,.1,.1, 1.0 )
                glUniform4f( self.Light_diffuse_loc, .25,.25,.25,1 )
                glUniform4f( self.Light_specular_loc, 0.0,1.0,0,1 )
                glUniform3f( self.Light_location_loc, 6,2,4 )
                glUniform4f( self.Material_ambient_loc, .1,.1,.1, 1.0 )
                glUniform4f( self.Material_diffuse_loc, .15,.15,.15, 1 )
                glUniform4f( self.Material_specular_loc, 1.0,1.0,1.0, 1.0 )
                glUniform1f( self.Material_shininess_loc, .95)

                glEnableVertexAttribArray(self.Vertex_position_loc)
                glEnableVertexAttribArray(self.Vertex_normal_loc)

                glVertexAttribPointer(self.Vertex_position_loc,
                3, GL_FLOAT, False, stride, self.coords)

                glVertexAttribPointer(self.Vertex_normal_loc,
                3, GL_FLOAT, False, stride, self.coords+(5*4))

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

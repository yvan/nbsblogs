# http://pyopengl.sourceforge.net/context/tutorials/shader_4.html
'''
conda create -n opengl-py2 python=2 -y
pip install TTFQuery PyOpenGL PyOpenGL-accelerate
pip install pydispatcher PyVRML97 PyVRML97-accelerate simpleparse
pip install OpenGLContext
conda install numpy
pip install Pillow
'''

'''
this tutorial is all about lighting,

diffuse lighting + ambient lighting only.

this photo explains ambient + diffuse + specular very well:

https://upload.wikimedia.org/wikipedia/commons/6/6b/Phong_components_version_4.png

ambient lighting -- is the light an object emits if there are no other lights
in the scene. it simulates the effect of light bouncing around theenvironment and
isnt accounted for by any other direct light sources. this makes sense. in the real world
light bounces off stuff  and everything has a

diffuse lighting (lambertian reflectance) is light that emits evenly in all directions
when alight hits the object. think if of the 'matte' finish you can get on things
or anti glare screens anti glare is actually an application of using diffuse lighting (via a material with high
diffusion).

eye space vs model space - basically when we pass values to phong wieght calc or wtvr we have 2 forms for the models
basically eye space is what your eye would see and model space is like if you dont multipley by the normals
most tutorials in eye space and its easier to reason about.

phong lighting - this takes a light position and a fragment normal takes their dot product (bounded at 0) and returns
the value

this vertex shader returns acolor the color is determined by the global ambiehnt light the ambient light contribution of
the point light we have (whyen we turn on our llight it increases ambient light as well).

the diffuse light value * materials duiffuse reflectance * the diffuse wieght which is returned from the phong lighting

normalize gets a vector in the same dirction as the original vector but the vector has a magnitude of 1.
see: https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/normalize.xhtml

the normal matrix: http://www.lighthouse3d.com/tutorials/glsl-12-tutorial/the-normal-matrix/
'''

from OpenGLContext import testingcontext
BaseContext = testingcontext.getInteractive()
from OpenGL.GL import *
from OpenGL.arrays import vbo
from OpenGLContext.arrays import *
from OpenGL.GL import shaders

class TestContext(BaseContext):
    def OnInit(self):
        # for ambient lighting
        phong_weightCalc = '''
        float phong_weightCalc(
            in vec3 light_pos,
            in vec3 frag_normal
        ){
            float n_dot_pos = max(0.0, dot(frag_normal, light_pos));
            return n_dot_pos;
        }
        '''

        vertex = '''
        uniform vec4 Global_ambient;
        uniform vec4 Light_ambient;
        uniform vec4 Light_diffuse;
        uniform vec3 Light_location;
        uniform vec4 Material_ambient;
        uniform vec4 Material_diffuse;
        attribute vec3 Vertex_position;
        attribute vec3 Vertex_normal;
        varying vec4 baseColor;
        void main(){
            gl_Position = gl_ModelViewProjectionMatrix * vec4(
                Vertex_position, 1.0
            );
            // gets the light into eye space, coordinates
            vec3 EC_Light_location = gl_NormalMatrix * Light_location;
            // calculate phong weight for vertex
            // by using its normal (in eye space) and the
            // eye space light position, dont totally
            // get this, norm them both so they are len(1)
            float diffuse_weight = phong_weightCalc(
                normalize(EC_Light_location),
                normalize(gl_NormalMatrix * Vertex_normal)
            );
            // get a 0-1 value for this vertex color
            // that is a combination of the global light
            // the ambient light from the light source
            // and the diffuse light
            baseColor = clamp(
            (
            // interaction with ambient global light
            (Global_ambient * Material_ambient)
            // interaction with ambient light
            // from light source
            +(Light_ambient * Material_ambient)
            // interaction with direct light
            +(Light_diffuse * Material_diffuse * diffuse_weight)
            ), 0.0, 1.0);
        }
        '''
        vertex_s = shaders.compileShader(phong_weightCalc + vertex, GL_VERTEX_SHADER)

        fragment = '''
        varying vec4 baseColor;
        void main() {
            //simply return the color from the vertex
            //shader using builtin fragment shader blending
            gl_FragColor = baseColor;
        }
        '''
        fragment_s = shaders.compileShader(fragment, GL_FRAGMENT_SHADER)
        self.shader = shaders.compileProgram(vertex_s, fragment_s)
        self.vbo = vbo.VBO(
            array( [
                [ -1, 0, 0, -1,0,1],
                [  0, 0, 1, -1,0,2],
                [  0, 1, 1, -1,0,2],
                [ -1, 0, 0, -1,0,1],
                [  0, 1, 1, -1,0,2],
                [ -1, 1, 0, -1,0,1],

                [  0, 0, 1, -1,0,2],
                [  1, 0, 1, 1,0,2],
                [  1, 1, 1, 1,0,2],
                [  0, 0, 1, -1,0,2],
                [  1, 1, 1, 1,0,2],
                [  0, 1, 1, -1,0,2],

                [  1, 0, 1, 1,0,2],
                [  2, 0, 0, 1,0,1],
                [  2, 1, 0, 1,0,1],
                [  1, 0, 1, 1,0,2],
                [  2, 1, 0, 1,0,1],
                [  1, 1, 1, 1,0,2],
            ],'f')
        )

        uniform_values = (
        'Global_ambient',
        'Light_ambient',
        'Light_diffuse',
        'Light_location',
        'Material_ambient',
        'Material_diffuse'
        )
        for uniform in uniform_values:
            location = glGetUniformLocation(self.shader, uniform)
            if location in (None, -1):
                print('Warning, no uniform {}'.format(uniform))
            else:
                set_attrib = uniform+'_loc'
                setattr(self, set_attrib, location)

        attribute_values = (
        'Vertex_position',
        'Vertex_normal'
        )
        for attribute in attribute_values:
            location = glGetAttribLocation(self.shader, attribute)
            if location in (None, -1):
                print('Warning, no attribute {}'.format(attribute))
            else:
                set_attrib = attribute+'_loc'
                setattr(self, set_attrib, location)

    def Render(self, mode=None):
        BaseContext.Render(self, mode)
        glUseProgram(self.shader)
        try:
            self.vbo.bind()
            try:
                glUniform4f( self.Global_ambient_loc, .3,.05,.05,.1 )
                glUniform4f( self.Light_ambient_loc, .2,.2,.2, 1.0 )
                glUniform4f( self.Light_diffuse_loc, 1,1,1,1 )
                glUniform3f( self.Light_location_loc, 2,2,10 )
                glUniform4f( self.Material_ambient_loc, .2,.2,.2, 1.0 )
                glUniform4f( self.Material_diffuse_loc, 1,1,1, 1 )

                glEnableVertexAttribArray(self.Vertex_position_loc)
                glEnableVertexAttribArray(self.Vertex_normal_loc)
                stride = 6 * 4
                glVertexAttribPointer(
                    self.Vertex_position_loc,
                    3, GL_FLOAT, False, stride, self.vbo
                )
                glVertexAttribPointer(
                    self.Vertex_normal_loc,
                    3, GL_FLOAT, False, stride, self.vbo+12
                )
                glDrawArrays(GL_TRIANGLES, 0, 18)
            finally:
                self.vbo.unbind()
                glDisableVertexAttribArray(self.Vertex_position_loc)
                glDisableVertexAttribArray(self.Vertex_normal_loc)
        finally:
            glUseProgram(0)

if __name__ == '__main__':
    TestContext.ContextMainLoop()

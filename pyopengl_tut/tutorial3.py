'''
http://pyopengl.sourceforge.net/context/tutorials/shader_3.html

uniform values are like global values, similarly to varying values they can be
used to share info between vert and frag shaders but they dont change
(as opposed to varying values which do). uniform values are the same
for the entire rendering call.

fttransform takes vertexes and transforms them to have the correct positions
instead of gl_ModelViewProjectionMatrix * gl_Vertex, its just more performant
'''

from OpenGLContext import testingcontext
BaseContext = testingcontext.getInteractive()
from OpenGL.GL import *
from OpenGL.arrays import vbo
from OpenGLContext.arrays import *
from OpenGL.GL import shaders

class TestContext(BaseContext):
    def OnInit(self):
        vertex = '''
        //declare 2 uniform value for the fog
        uniform float end_fog;
        uniform vec4 fog_color;

        void main() {
            float fog;
            float fog_coord;

            //calculate positions
            gl_Position = ftransform();

            //get the ddistance along z
            fog_coord = abs(gl_Position.z);

            //set it to be on the scale of the fog
            fog_coord = clamp(fog_coord, 0.0, end_fog);

            //caluclate how much fog
            fog = (end_fog - fog_coord)/end_fog;
            fog = clamp(fog, 0.0, 1.0);

            //pass result to this which goes into gl_Color
            gl_FrontColor = mix(fog_color, gl_Color, fog);
        }
        '''
        vertex_s = shaders.compileShader(vertex, GL_VERTEX_SHADER)
        fragment = '''
        void main() {
            gl_FragColor = gl_Color;
        }
        '''
        fragment_s = shaders.compileShader(fragment, GL_FRAGMENT_SHADER)
        self.shader = shaders.compileProgram(vertex_s, fragment_s)
        self.vbo = vbo.VBO(
            array([
                [  0, 1, 0,  0,1,0 ],
                [ -1,-1, 0,  1,1,0 ],
                [  1,-1, 0,  0,1,1 ],
                [  2,-1, 0,  1,0,0 ],
                [  4,-1, 0,  0,1,0 ],
                [  4, 1, 0,  0,0,1 ],
                [  2,-1, 0,  1,0,0 ],
                [  4, 1, 0,  0,0,1 ],
                [  2, 1, 0,  0,1,1 ],
            ], 'f')
        )
        # get the locations in shader memory for our two uniform values
        self.UNIFORM_LOCATIONS = {
            'end_fog': glGetUniformLocation(self.shader, 'end_fog'),
            'fog_color': glGetUniformLocation(self.shader, 'fog_color')
        }

    def Render(self, mode=0):
        BaseContext.Render(self, mode)
        glUseProgram(self.shader)
        
        # where we actually pass in values to the fog uniform values
        # switched my fog to 0000 from 1111 black to make it blend in
        # with background
        glUniform1f(self.UNIFORM_LOCATIONS['end_fog'], 15)
        glUniform4f(self.UNIFORM_LOCATIONS['fog_color'], 0, 0, 0, 0)

        # rotate the triangle and zoom in
        glRotate(45,0,1,0)
        glScale(3,3,3)
        try:
            self.vbo.bind()
            try:
                glEnableClientState(GL_VERTEX_ARRAY)
                glEnableClientState(GL_COLOR_ARRAY)
                glVertexPointer(3, GL_FLOAT, 24, self.vbo)
                glColorPointer(3, GL_FLOAT, 24, self.vbo+12)
                glDrawArrays(GL_TRIANGLES, 0, 9)
            finally:
                self.vbo.unbind()
                glDisableClientState(GL_VERTEX_ARRAY)
                glDisableClientState(GL_COLOR_ARRAY)
        finally:
            glUseProgram(0)

if __name__ == '__main__':
    TestContext.ContextMainLoop()

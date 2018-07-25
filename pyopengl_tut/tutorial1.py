'''
http://pyopengl.sourceforge.net/context/tutorials/shader_1.html

you use glVertexPointer to point at your data in teh VBO. you gotta bind your
VBO data into the gpu

glDrawArrays used to draw vertices. the vertex shader plugs in the points the
fragment shader colors fragments between the points

vertex shader needs to return gl_Position fragment shader needs to return
gl_FragColor

gl_ModelViewProjectionMatrix just says to multiply our vertices by whatever
numbers to get the viewpoint right given our vertices in the vbo

you can only draw vertices / triangles with GL_TRIANGLES
'''

# this is for creating a window
from OpenGLContext import testingcontext
BaseContext = testingcontext.getInteractive()

# import opengl
from OpenGL.GL import *

# import the vertext buffer object
from OpenGL.arrays import vbo

# opengl context just imports numpy or alternative
from OpenGLContext.arrays import *

# get shaders
from OpenGL.GL import shaders

# create the class that runs the opengl context
class TestContext(BaseContext):
    def OnInit(self):
        # create a vertex and fragment shader
        # apparently verterx shaders only need
        # to return a gl_Position, an fragment
        # shaders only need to return a color
        vshader = '''#version 120
        void main(){
            gl_Position = gl_ModelViewProjectionMatrix * gl_Vertex;
        }
        '''
        fshader = '''#version 120
        void main(){
            gl_FragColor = vec4(0,1,0,1);
        }
        '''
        # compile our shaders
        VERTEX_SHADER = shaders.compileShader(vshader, GL_VERTEX_SHADER)
        FRAGMENT_SHADER = shaders.compileShader(fshader, GL_FRAGMENT_SHADER)
        self.shader = shaders.compileProgram(VERTEX_SHADER, FRAGMENT_SHADER)
        # create a vbo (stored actual data for our triangles)
        self.vbo = vbo.VBO(
            array([
                [  0, 1, 0 ],
                [ -1,-1, 0 ],
                [  1,-1, 0 ],
                [  2,-1, 0 ],
                [  4,-1, 0 ],
                [  4, 1, 0 ],
                [  2,-1, 0 ],
                [  4, 1, 0 ],
                [  2, 1, 0 ],
            ],'f')
        )


    def Render(self, mode):
        '''render the geometry of the scene'''
        # tell opengl to use our shader
        shaders.glUseProgram(self.shader)
        try:
            # bind data into gpu
            self.vbo.bind()
            try:
                # tells opengl to access vertex once
                # we call a draw function
                glEnableClientState(GL_VERTEX_ARRAY)
                # point at our vbo data
                glVertexPointerf(self.vbo)
                # actually tell opengl to draw
                # the stuff in the VBO as a series
                # of triangles
                glDrawArrays(GL_TRIANGLES, 0, 9)
            finally:
                # cleanup, unbind the our data from gpu ram
                # and tell opengl that it should not
                # expect vertex arrays anymore
                self.vbo.unbind()
                glDisableClientState(GL_VERTEX_ARRAY)
        finally:
            # stop using our shader
            shaders.glUseProgram(0)

if __name__ == '__main__':
    # run our shader loop
    TestContext.ContextMainLoop()

# http://pyopengl.sourceforge.net/context/tutorials/shader_1.html
'''
conda create -n opengl-py2 python=2 -y
pip install TTFQuery PyOpenGL PyOpenGL-accelerate
pip install pydispatcher PyVRML97 PyVRML97-accelerate simpleparse
pip install OpenGLContext
conda install numpy
pip install Pillow
'''

'''
main takeaway

you can pack color data and position data in the same arrays
then give offsets to glVertexPointer and glColorPointer so thery only ever refernce
certain portions of the data in the vbo (i.e. always first 3)

gl_Color is a legacy value that shows up to grab the color of each vertex in the vertex shader OpenGL
does this automatically /(probably via the gl color pointer)

we use a varying type value here this just alloqs us to pass a global value between
vert and fragment shaders

i guess if we give our fragment shader each vertex color it just auto interpolates between.

you can think of the fragment shader as getting 3 vertexes and then choosing colors for the poitns
in between

in  otherwords the vertex shader is amapping function from verts to points in spaceself.
the fragment shader is a mapping function from multiple points in space to a region of points, usually
bound by the input points in space somehowself.

fragment shaders are called repeatedly and so you want to keep computation light there

'''

from OpenGLContext import testingcontext
BaseContext = testingcontext.getInteractive()
from OpenGL.GL import *
from OpenGL.arrays import vbo
from OpenGLContext.arrays import *
from OpenGL.GL import shaders

class TestContext(BaseContext):
    def OnInit(self):
        vertex_s = '''
        varying vec4 vertex_color;
        void main(){
            gl_Position = gl_ModelViewProjectionMatrix * gl_Vertex;
            vertex_color = gl_Color;
        }
        '''
        vertex = shaders.compileShader(vertex_s, GL_VERTEX_SHADER)
        fragment_s = '''
        varying vec4 vertex_color;
        void main(){
            gl_FragColor = vertex_color;
        }
        '''
        fragment = shaders.compileShader(fragment_s, GL_FRAGMENT_SHADER)
        self.shader = shaders.compileProgram(vertex, fragment)
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
    def Render(self, mode):
        BaseContext.Render(self,mode)
        glUseProgram(self.shader)
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

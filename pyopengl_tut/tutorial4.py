'''
http://pyopengl.sourceforge.net/context/tutorials/shader_4.html

glVertexPointer and glColorPointer are legacy opengl. we can just use arbitrary
glEnableVerexAttribArray + a glVertexAttribPointer to point at the data.

if you misspell glEnableVerexAttribArray the program will run, close and not
throw any errors.

the whole point of this tuorial is to set sme uniform value (the tween) that
tells our vertex shader how to 'combine' two points to render some point in
between for an animation.

a tweened position is just a secondary position that gets blended with the
primary vertex position to produce some value in between
'''

from OpenGLContext import testingcontext
BaseContext = testingcontext.getInteractive()
from OpenGL.GL import *
from OpenGL.arrays import vbo
from OpenGLContext.arrays import *
from OpenGL.GL import shaders
from OpenGLContext.events.timer import Timer

class TestContext(BaseContext):
    def OnInit(self):
        vshader = '''
        //setup a tween (blending constant)
        uniform float tween;
        // the the original position
        // and the secondary position (tweened)
        attribute vec3 position;
        attribute vec3 tweened;
        attribute vec3 color;
        varying vec4 baseColor;

        void main(){
            // create a new position for this vertex
            // by mixing the original position and the
            // secondary tweened position
            gl_Position = gl_ModelViewProjectionMatrix * mix(
                vec4(position, 1.0),
                vec4(tweened, 1.0),
                tween
            );
            baseColor = vec4(color, 1.0);
        }
        '''
        vertex = shaders.compileShader(vshader, GL_VERTEX_SHADER)
        fshader = '''
        varying vec4 baseColor;
        void main(){
            gl_FragColor = baseColor;
        }
        '''
        fragment = shaders.compileShader(fshader, GL_FRAGMENT_SHADER)
        self.shader = shaders.compileProgram(vertex, fragment)

        # create a vbo with vertex position, color, and secondary position
        self.vbo = vbo.VBO(
            array([
                [  0, 1, 0, 1,3,0,  0,1,0 ],
                [ -1,-1, 0, -1,-1,0,  1,1,0 ],
                [  1,-1, 0, 1,-1,0, 0,1,1 ],
                [  2,-1, 0, 2,-1,0, 1,0,0 ],
                [  4,-1, 0, 4,-1,0, 0,1,0 ],
                [  4, 1, 0, 4,9,0, 0,0,1 ],
                [  2,-1, 0, 2,-1,0, 1,0,0 ],
                [  4, 1, 0, 1,3,0, 0,0,1 ],
                [  2, 1, 0, 1,-1,0, 0,1,1 ],
            ], 'f')
        )
        # get the memory locations in our shader
        # for our variables
        self.position_location = glGetAttribLocation(
            self.shader, 'position'
        )
        self.tweened_location = glGetAttribLocation(
            self.shader, 'tweened'
        )
        self.color_location = glGetAttribLocation(
            self.shader, 'color'
        )
        self.tween_location = glGetUniformLocation(
            self.shader, 'tween'
        )

        # setup an animation timer which updates the
        # tween fraction (called tween in our shader)
        self.time = Timer(duration=2.0, repeating=1)
        self.time.addEventHandler('fraction', self.OnTimerFraction)
        self.time.register(self)
        self.time.start()

    def Render(self, mode=0):
        BaseContext.Render(self, mode)
        glUseProgram(self.shader)
        # put the tween_fraction from the animation callback
        # into the shader's variable
        glUniform1f(self.tween_location, self.tween_fraction)
        try:
            self.vbo.bind()
            try:
                glEnableVertexAttribArray(self.position_location)
                glEnableVertexAttribArray(self.tweened_location)
                glEnableVertexAttribArray(self.color_location)
                # point to our position, color, and secondary
                # position as vertex attrib pointers, the
                # +12, +24 just say to start at the 4th
                # and 7th columns respectively because
                # columns 0-2 are vertex positions,
                # columns 3-5 are secondary positions
                # columns 6-8 are colors for the vertices
                # the stride is how many bytes to jump over
                # this just make sure we skip coulmns that
                # contain other data (say color/secondary position)
                stride = 9*4
                glVertexAttribPointer(
                    self.position_location,
                    3,GL_FLOAT,False,stride,self.vbo
                )
                glVertexAttribPointer(
                    self.tweened_location,
                    3, GL_FLOAT,False, stride, self.vbo+12
                )
                glVertexAttribPointer(
                    self.color_location,
                    3, GL_FLOAT,False, stride, self.vbo+24
                )
                glDrawArrays(GL_TRIANGLES, 0, 9)
            finally:
                self.vbo.unbind()
                glDisableVertexAttribArray( self.position_location )
                glDisableVertexAttribArray( self.tweened_location )
                glDisableVertexAttribArray( self.color_location )
        finally:
            glUseProgram(0)

    tween_fraction = 0.0

    def OnTimerFraction(self, event):
        frac = event.fraction()
        if frac > .5:
            frac = 1.0-frac
        frac *= 2
        self.tween_fraction = frac
        self.triggerRedraw()


if __name__ == '__main__':
    TestContext.ContextMainLoop()

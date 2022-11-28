
const cnv = document.getElementById("cnv");
const ctx = cnv.getContext("experimental-webgl");

var g = {};

function init()
{
    // Initialize
    var gl = initWebGL("cnv");
    if (!gl) {
        return;
    }

    var program = simpleSetup(
            gl, "vshader", "fshader", 
            [ "vNormal", "vColor", "vPosition"], [ 0.3, 0.7, 1.0, 1 ], 10000);

    // Create a quad. with the BufferObjects containing the arrays 
    // for vertices, normals, texture coords, and indices.
    g.box = makeScreenQuad(gl);

    // Set some uniform variables for the shaders
	g.angle = 0;
	g.u_angle = gl.getUniformLocation(program, "angle");
	gl.uniform1f(g.u_angle, g.angle);

    g.projectionMatrix = new J3DIMatrix4();
    g.projectionMatrix.ortho(-1, 1, -1, 1, -1, 10000);
    g.u_projectionMatrix = gl.getUniformLocation(program, "u_ProjectionMatrix");

    // Enable all of the vertex attribute arrays.
    gl.enableVertexAttribArray(0);
    gl.enableVertexAttribArray(1);
    gl.enableVertexAttribArray(2);

    // Set up all the vertex attributes for vertices, normals and texCoords
    gl.bindBuffer(gl.ARRAY_BUFFER, g.box.normalObject);
    gl.vertexAttribPointer(0, 3, gl.FLOAT, false, 0, 0);

    gl.bindBuffer(gl.ARRAY_BUFFER, g.box.texCoordObject);
    gl.vertexAttribPointer(1, 2, gl.FLOAT, false, 0, 0);

    gl.bindBuffer(gl.ARRAY_BUFFER, g.box.vertexObject);
    gl.vertexAttribPointer(2, 3, gl.FLOAT, false, 0, 0);

    // Bind the index array
    gl.bindBuffer(gl.ELEMENT_ARRAY_BUFFER, g.box.indexObject);

    return gl;
}

var hasReshaped = false;
function reshape(gl)
{
    // if the display size of the canvas has changed
    // change the size we render at to match.
    var canvas = document.getElementById('cnv');
    if (hasReshaped && canvas.clientWidth == canvas.width && canvas.clientHeight == canvas.height) {
        return;
    }
	hasReshaped = true;

    canvas.width = canvas.clientWidth;
    canvas.height = canvas.clientHeight;

    // Set the viewport and projection matrix for the scene
    gl.viewport(0, 0, canvas.clientWidth, canvas.clientHeight);
}

function drawPicture(gl)
{
    //Make sure the canvas is sized correctly.
    reshape(gl);

    // Clear the canvas
    gl.clear(gl.COLOR_BUFFER_BIT | gl.DEPTH_BUFFER_BIT);

    g.projectionMatrix.setUniform(gl, g.u_projectionMatrix, false);

	gl.uniform1f(g.u_angle, g.angle);

    // Draw the cube
    gl.drawElements(gl.TRIANGLES, g.box.numIndices, gl.UNSIGNED_BYTE, 0);

}

function start() {
	cnv.addEventListener('webglcontextlost', handleContextLost, false);
	cnv.addEventListener('webglcontextrestored', handleContextRestored, false);

	var gl = init();
	if (!gl) return;

	var frameDelay = 1000 / 10;
	var then = Date.now() - frameDelay;

	var f = function() {
		g.angle += 0.0003;
		var now = Date.now();
		var elapsed = now - then;
		if (elapsed > frameDelay) {
			then = now - (elapsed % frameDelay);
			drawPicture(gl);
		}
		requestId = window.requestAnimFrame(f, cnv);
	};
	f();

	function handleContextLost(e) {
		e.preventDefault();
		clearLoadingImages();
		if (requestId !== undefined) {
			window.cancelAnimFrame(requestId);
			requestId = undefined;
		}
	}

	function handleContextRestored() {
		init();
		f();
	}

}

start();

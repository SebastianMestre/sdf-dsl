
const cnv = document.getElementById("cnv");
const ctx = cnv.getContext("experimental-webgl");
const btnDibujar = document.getElementById("btn-dibujar");
const textShader = document.getElementById("text-shader");
const scriptFshader = document.getElementById("fshader");

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
	if (!gl) return null;

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

	return {
		refresh() {
			gl = init();
		},
	};

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

var runner = start();

btnDibujar.addEventListener("click", () => {
	scriptFshader.text = renderFragShader(textShader.value);
	runner.refresh();
});



function renderFragShader(sdfContent) {

return `
    precision mediump float;


	uniform float angle;

    varying vec2 v_texCoord;

	const vec3 sky_color = vec3(0.7, 0.8, 1.0) * 0.2;
	const vec3 sky_direction = vec3(0.0, 1.0, 0.0);

	float sdf1(vec3 pos) {
		${ sdfContent }
	}

	float sdf(vec3 pos, mat3 transform) {
		pos = transform * pos;
		float scale = 10.0;
		return sdf1((pos + vec3(0.0, 1.5, 0.0)) / scale) * scale;
	}

	vec4 raymarch(vec3 pos, vec3 dir, mat3 transform) {

		float t = 1.0;
		for (int i = 0; i < 70; ++i) {
			float dist = sdf(pos + dir * t, transform);
			if (dist < 0.0001 * t) {
				return vec4(pos + dir * t, 1.0);
			}
			t += dist;
		}

		return vec4(pos, 0.0);
	}

	vec4 view_raymarch(vec3 dir, mat3 transform) {
		return raymarch(vec3(0.0, 0.0, -5.0), dir, transform);
	}

	vec3 compute_normal(vec3 p, mat3 A) {
		const float h = 0.0001;
		const vec3 k = vec3(1.0, -1.0, 0.0);
		// return normalize(
		// k.xzz * (sdf(p+k.xzz*h, A) - sdf(p-k.xzz*h, A)) +
		// k.zxz * (sdf(p+k.zxz*h, A) - sdf(p-k.zxz*h, A)) +
		// k.zzx * (sdf(p+k.zzx*h, A) - sdf(p-k.zzx*h, A)) );
		return normalize(
			k.xyy * sdf(p+k.xyy*h, A) +
			k.yyx * sdf(p+k.yyx*h, A) +
			k.yxy * sdf(p+k.yxy*h, A) +
			k.xxx * sdf(p+k.xxx*h, A) );
	}

	vec3 linear_to_gamma(vec3 color) {
		float level = 1.0 / 2.2;
		return vec3(
			pow(color.x, level),
			pow(color.y, level),
			pow(color.z, level));
	}

	vec3 gamma_to_linear(vec3 color) {
		float level = 2.2;
		return vec3(
			pow(color.x, level),
			pow(color.y, level),
			pow(color.z, level));
	}

	float blinn_phong(vec3 n, vec3 l, vec3 v, float exponent) {
		vec3 h = normalize(l + v);
		float normalization = (exponent + 1.0) * 0.15915494309;
		float intensity = pow(max(dot(n, h), 0.0), exponent);
		return intensity * normalization;
	}

	float lambert(vec3 n, vec3 l) {
		return max(dot(n, l), 0.0) * 0.31830988618;
	}

	float wide_lambert(vec3 n, vec3 l) {
		return dot(n, l) * 0.5 + 0.5;
	}

	float schlick(vec3 n, vec3 v) {
		float curve = pow(1.0 - dot(n, v), 5.0);
		float base = 0.04;
		return mix(base, 1.0, curve);
	}

	float soft_visiblity(vec3 pos, vec3 dir, mat3 transform, float sharpness) {

		float res = 1.0;
		float t = 0.02;

		for (int i = 0; i < 128; ++i) {
			float dist = sdf(pos + dir * t, transform );
			if (dist < 0.0001) return 0.0;
			res = min(res, dist / (t * sharpness));
			t += dist;
		}

		return res;
	}

	vec3 shade(vec3 pos, vec3 n, vec3 v, mat3 transform) {

		vec3 sky_power = 1.0 * gamma_to_linear(sky_color);
		vec3 sky_diffuse = sky_power * wide_lambert(n, sky_direction);
		vec3 sky_specular = sky_power * 1.0;

		vec3 lamp1_dir = normalize(vec3(1.0, 0.5, -0.3));
		vec3 lamp1_color = vec3(0.96, 0.83, 0.51) * 0.0 + 1.0;
		vec3 lamp1_power = 10.0 * gamma_to_linear(lamp1_color) * soft_visiblity(pos, lamp1_dir, transform, 0.1);
		vec3 lamp1_diffuse = lamp1_power * vec3(lambert(n, lamp1_dir));
		vec3 lamp1_specular = lamp1_power * vec3(blinn_phong(n, lamp1_dir, v, 34.0));

		vec3 lamp2_dir = normalize(vec3(-1.0, 0.1, -0.2));
		vec3 lamp2_color = vec3(0.96, 0.83, 0.51);
		vec3 lamp2_power = 0.2 * gamma_to_linear(lamp2_color);
		vec3 lamp2_diffuse = lamp2_power * vec3(wide_lambert(n, lamp2_dir));
		vec3 lamp2_specular = lamp2_power * vec3(blinn_phong(n, lamp2_dir, v, 8.0));

		vec3 lamp3_dir = normalize(vec3(-0.4, -0.1, 0.3));
		vec3 lamp3_color = vec3(0.2, 0.7, 0.5);
		vec3 lamp3_power = 0.5 * gamma_to_linear(lamp3_color);
		vec3 lamp3_diffuse = lamp3_power * vec3(lambert(n, lamp3_dir));
		vec3 lamp3_specular = lamp3_power * vec3(blinn_phong(n, lamp3_dir, v, 15.0));

		float fresnel = schlick(n, v);

		vec3 color = vec3(1.0, 0.8, 0.3);
		vec3 albedo = gamma_to_linear(color);
		vec3 diffuse = albedo * (lamp1_diffuse + lamp2_diffuse + lamp3_diffuse + sky_diffuse);

		vec3 specular = lamp1_specular + lamp2_specular + lamp3_specular + 0.6 * sky_specular;

		return mix(diffuse, specular, fresnel);
	}

	void main() {
		float ca = cos(angle);
		float sa = sin(angle);
		mat3 transform = mat3(
			ca, 0, -sa,
			 0, 1,   0,
			sa, 0,  ca
		);

		vec2 rectCoord = (v_texCoord.st * 2.0 - 1.0) * 0.5;

		vec3 v = -normalize(vec3(rectCoord, 1.0));

		vec4 res = view_raymarch(-v, transform);
		if (res.a < 0.5) {
			gl_FragColor = vec4(sky_color, 1.0);
		} else {
			vec3 n = compute_normal(res.xyz, transform);
			vec3 light = shade(res.xyz, n, v, transform);
			vec3 tonemapped = light / (light + 1.0);
			gl_FragColor = vec4(linear_to_gamma(tonemapped), 1.0);
			// gl_FragColor = vec4(linear_to_gamma(n*0.5+0.5), 1.0);
		}
	}
	`;




}

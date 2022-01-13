import init, * as Truck from "./pkg/truck_param_js.js";

const fps = 1000 / 30;

class App {
  #canvas;
  #gl;

  #mouse = [0.0, 0.0];
  #rotflag = false;
  #cameraPosition = [0.0, 0.0, 3.0];
  #cameraDirection = [0.0, 0.0, -1.0];
  #cameraUpdirection = [0.0, 1.0, 0.0];
  #cameraGaze = [0.0, 0.0, 0.0];

  #loaded = false;
  #needsRender = true;

  #vAttributes;
  #vIndex;

  #vPositionLocation;
  #vUVLocation;
  #vNormalLocation;

  #uniLocation;

  #vBuffer;
  #iBuffer;
  #indexLength;

  constructor(canvas) {
    canvas.addEventListener("mousemove", this.#mouseMove.bind(this));
    canvas.addEventListener("mousedown", this.#mouseDown.bind(this));
    canvas.addEventListener("mouseup", this.#mouseUp.bind(this));

    this.#gl =
      canvas.getContext("webgl2") || canvas.getContext("experimental-webgl");

    const prg = this.#createProgram(
      this.#createShader("vertexshader"),
      this.#createShader("fragmentshader")
    );
    this.#uniLocation = [
      this.#gl.getUniformLocation(prg, "camera_position"),
      this.#gl.getUniformLocation(prg, "camera_direction"),
      this.#gl.getUniformLocation(prg, "camera_updirection"),
      this.#gl.getUniformLocation(prg, "resolution"),
    ];

    this.#gl.enable(this.#gl.CULL_FACE);
    this.#gl.enable(this.#gl.DEPTH_TEST);

    this.#vPositionLocation = this.#gl.getAttribLocation(prg, "position");
    this.#vUVLocation = this.#gl.getAttribLocation(prg, "uv");
    this.#vNormalLocation = this.#gl.getAttribLocation(prg, "normal");

    this.#gl.clearColor(46 / 455, 52 / 455, 64 / 455, 1.0);
    this.#gl.clearDepth(1.0);

    this.#canvas = canvas;

    this.#render();
  }

  #createProgram(vs, fs) {
    const program = this.#gl.createProgram();

    this.#gl.attachShader(program, vs);
    this.#gl.attachShader(program, fs);

    this.#gl.linkProgram(program);

    if (this.#gl.getProgramParameter(program, this.#gl.LINK_STATUS)) {
      this.#gl.useProgram(program);
      return program;
    } else {
      alert(this.#gl.getProgramInfoLog(program));
    }
  }

  #createShader(id) {
    let shader;

    const scriptElement = document.getElementById(id);
    if (!scriptElement) return;

    switch (scriptElement.type) {
      case "x-shader/x-vertex":
        shader = this.#gl.createShader(this.#gl.VERTEX_SHADER);
        break;
      case "x-shader/x-fragment":
        shader = this.#gl.createShader(this.#gl.FRAGMENT_SHADER);
        break;
      default:
        return;
    }

    this.#gl.shaderSource(shader, scriptElement.text);
    this.#gl.compileShader(shader);

    if (this.#gl.getShaderParameter(shader, this.#gl.COMPILE_STATUS)) {
      return shader;
    } else {
      alert(this.#gl.getShaderInfoLog(shader));
    }
  }

  #createVbo(data) {
    const vbo = this.#gl.createBuffer();
    this.#gl.bindBuffer(this.#gl.ARRAY_BUFFER, vbo);
    this.#gl.bufferData(this.#gl.ARRAY_BUFFER, data, this.#gl.STATIC_DRAW);
    this.#gl.bindBuffer(this.#gl.ARRAY_BUFFER, null);
    return vbo;
  }

  #createIbo(data) {
    const ibo = this.#gl.createBuffer();
    this.#gl.bindBuffer(this.#gl.ELEMENT_ARRAY_BUFFER, ibo);
    this.#gl.bufferData(
      this.#gl.ELEMENT_ARRAY_BUFFER,
      new Uint16Array(data),
      this.#gl.STATIC_DRAW
    );
    this.#gl.bindBuffer(this.#gl.ELEMENT_ARRAY_BUFFER, null);
    return ibo;
  }

  #render() {
    if (this.#loaded) {
      this.#vAttributes = this.#createVbo(this.#vBuffer);
      this.#vIndex = this.#createIbo(this.#iBuffer);
      this.#loaded = false;
      this.#needsRender = true;
    }

    if (!this.#needsRender) {
      setTimeout(this.#render.bind(this), fps);
      return;
    }

    let gl = this.#gl;
    let c = this.#canvas;

    gl.clear(gl.COLOR_BUFFER_BIT | gl.DEPTH_BUFFER_BIT);

    if (!this.#vBuffer) {
      gl.flush();
      this.#needsRender = false;
      setTimeout(this.#render.bind(this), fps);
      return;
    }

    gl.uniform3fv(this.#uniLocation[0], this.#cameraPosition);
    gl.uniform3fv(this.#uniLocation[1], this.#cameraDirection);
    gl.uniform3fv(this.#uniLocation[2], this.#cameraUpdirection);
    gl.uniform2fv(this.#uniLocation[3], [c.width, c.height]);

    gl.bindBuffer(gl.ARRAY_BUFFER, this.#vAttributes);
    if (this.#vPositionLocation >= 0) {
      gl.enableVertexAttribArray(this.#vPositionLocation);
      gl.vertexAttribPointer(
        this.#vPositionLocation,
        3,
        gl.FLOAT,
        false,
        3 * 4 + 2 * 4 + 3 * 4,
        0
      );
    }
    if (this.#vUVLocation >= 0) {
      gl.enableVertexAttribArray(this.#vUVLocation);
      gl.vertexAttribPointer(
        this.#vUVLocation,
        2,
        gl.FLOAT,
        false,
        3 * 4 + 2 * 4 + 3 * 4,
        3 * 4
      );
    }
    if (this.#vNormalLocation >= 0) {
      gl.enableVertexAttribArray(this.#vNormalLocation);
      gl.vertexAttribPointer(
        this.#vNormalLocation,
        3,
        gl.FLOAT,
        false,
        3 * 4 + 2 * 4 + 3 * 4,
        2 * 4 + 3 * 4
      );
    }

    gl.bindBuffer(gl.ELEMENT_ARRAY_BUFFER, this.#vIndex);

    gl.drawElements(gl.TRIANGLES, this.#indexLength, gl.UNSIGNED_SHORT, 0);

    gl.flush();

    this.#needsRender = false;
    setTimeout(this.#render.bind(this), fps);
  }

  #rotation(origin, axis, theta, vec) {
    vec = [vec[0] - origin[0], vec[1] - origin[1], vec[2] - origin[2]];
    vec = [
      (axis[0] * axis[0] * (1.0 - Math.cos(theta)) + Math.cos(theta)) * vec[0] +
        (axis[0] * axis[1] * (1.0 - Math.cos(theta)) -
          axis[2] * Math.sin(theta)) *
          vec[1] +
        (axis[0] * axis[2] * (1.0 - Math.cos(theta)) +
          axis[1] * Math.sin(theta)) *
          vec[2],
      (axis[0] * axis[1] * (1.0 - Math.cos(theta)) +
        axis[2] * Math.sin(theta)) *
        vec[0] +
        (axis[1] * axis[1] * (1.0 - Math.cos(theta)) + Math.cos(theta)) *
          vec[1] +
        (axis[1] * axis[2] * (1.0 - Math.cos(theta)) -
          axis[0] * Math.sin(theta)) *
          vec[2],
      (axis[0] * axis[2] * (1.0 - Math.cos(theta)) -
        axis[1] * Math.sin(theta)) *
        vec[0] +
        (axis[1] * axis[2] * (1.0 - Math.cos(theta)) +
          axis[0] * Math.sin(theta)) *
          vec[1] +
        (axis[2] * axis[2] * (1.0 - Math.cos(theta)) + Math.cos(theta)) *
          vec[2],
    ];
    return [vec[0] + origin[0], vec[1] + origin[1], vec[2] + origin[2]];
  }

  #mouseMove(e) {
    const offset = [e.offsetX, e.offsetY];
    if (this.#rotflag) {
      const diff = [offset[0] - this.#mouse[0], this.#mouse[1] - offset[1]];
      if (diff[0] == 0 || diff[1] == 0) return;
      diff[0] *= 0.01;
      diff[1] *= 0.01;
      const cameraRightdirection = [
        this.#cameraDirection[1] * this.#cameraUpdirection[2] -
          this.#cameraDirection[2] * this.#cameraUpdirection[1],
        this.#cameraDirection[2] * this.#cameraUpdirection[0] -
          this.#cameraDirection[0] * this.#cameraUpdirection[2],
        this.#cameraDirection[0] * this.#cameraUpdirection[1] -
          this.#cameraDirection[1] * this.#cameraUpdirection[0],
      ];
      const axis = [
        diff[0] * this.#cameraUpdirection[0] -
          diff[1] * cameraRightdirection[0],
        diff[0] * this.#cameraUpdirection[1] -
          diff[1] * cameraRightdirection[1],
        diff[0] * this.#cameraUpdirection[2] -
          diff[1] * cameraRightdirection[2],
      ];
      const len = Math.sqrt(
        axis[0] * axis[0] + axis[1] * axis[1] + axis[2] * axis[2]
      );
      axis[0] /= len;
      axis[1] /= len;
      axis[2] /= len;
      this.#cameraPosition = this.#rotation(
        this.#cameraGaze,
        axis,
        -len,
        this.#cameraPosition
      );
      this.#cameraDirection = this.#rotation(
        [0.0, 0.0, 0.0],
        axis,
        -len,
        this.#cameraDirection
      );
      this.#cameraUpdirection = this.#rotation(
        [0.0, 0.0, 0.0],
        axis,
        -len,
        this.#cameraUpdirection
      );
    }
    this.#mouse = offset;
    this.#needsRender = true;
  }

  #mouseDown(_e) {
    this.#rotflag = true;
  }

  #mouseUp(_e) {
    this.#rotflag = false;
  }

  draw(polygon) {
    if (typeof polygon === "undefined") {
      console.warn("meshing failed");
      return;
    }
    const box = polygon.bounding_box();
    const boxCenter = [
      (box[0] + box[3]) / 2.0,
      (box[1] + box[4]) / 2.0,
      (box[2] + box[5]) / 2.0,
    ];
    this.#cameraPosition = [
      this.#cameraPosition[0] - this.#cameraGaze[0] + boxCenter[0],
      this.#cameraPosition[1] - this.#cameraGaze[1] + boxCenter[1],
      this.#cameraPosition[2] - this.#cameraGaze[2] + boxCenter[2],
    ];
    this.#cameraGaze = boxCenter;
    const object = polygon.to_buffer();
    this.#vBuffer = object.vertex_buffer();
    this.#iBuffer = object.index_buffer();
    this.#indexLength = object.index_buffer_size() / 4;
    this.#loaded = true;
  }
}

async function run() {
  await init();

  const cw = 768;
  const ch = 768;

  let app;

  if (document.readyState !== "loading") {
    onLoad();
  } else {
    addEventListener("load", onLoad, false);
  }

  function onLoad() {
    let c = document.getElementById("canvas");
    c.width = cw;
    c.height = ch;

    app = new App(c);

    document.querySelector("input").addEventListener("change", fileRead);
  }

  function fileRead(e) {
    if (typeof e.target.value === "undefined") {
      console.warn("invalid input");
      return;
    }
    const solid = Truck.build_torus(0.5, e.target.value);

    if (typeof solid === "undefined") {
      console.warn("invalid json");
      return;
    }
    let polygon = solid.to_polygon(0.01);
    app.draw(polygon);
  }
}

run();

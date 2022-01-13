import init, * as Truck from "./pkg/truck_param_js.js";

var vertexShader = `#version 300 es
  in vec3 position;
  in vec2 uv;
  in vec3 normal;

  uniform vec3 camera_position;
  uniform vec3 camera_direction;
  uniform vec3 camera_updirection;
  uniform vec2 resolution;

  const float camera_fov = 3.141592653 / 4.0;
  const float camera_near = 0.1;
  const float camera_far = 10.0;

  out vec3 vertex_position;
  out vec2 vertex_uv;
  out vec3 vertex_normal;

  vec4 get_vertex_position(
      in vec3 pos,
      in vec3 c_pos,
      in vec3 c_dir,
      in vec3 c_up,
      in float c_fov,
      in float asp,
      in float c_far,
      in float c_near
  ) {
      vec3 vec = pos - c_pos;
      float far = 1.0 / tan(c_fov / 2.0);
      vec3 x_axis = cross(c_dir, c_up);
      vec3 y_axis = c_up;
      float depth = dot(c_dir, vec);
      vec3 h = (vec - depth * c_dir) * far;
      float u = dot(h, x_axis) / asp;
      float v = dot(h, y_axis);
      return vec4(u, v, (depth - c_near) / (c_far - c_near), depth);
  }

  void main() {
      gl_Position = get_vertex_position(
          position,
          camera_position,
          camera_direction,
          camera_updirection,
          camera_fov,
          resolution.x / resolution.y,
          camera_far,
          camera_near
      );
      vertex_position = position;
      vertex_uv = uv;
      vertex_normal = normal;
  }
`;

var fragmentShader = `#version 300 es
  precision highp float;

  in vec3 vertex_position;
  in vec2 vertex_uv;
  in vec3 vertex_normal;

  uniform vec3 camera_position;
  uniform vec3 camera_direction;
  uniform vec3 camera_updirection;
  uniform vec2 resolution;

  out vec4 color;

  // Based on the microfacet theory
  // cf: https://qiita.com/mebiusbox2/items/e7063c5dfe1424e0d01a

  struct Light {
      vec4 position;
      vec4 color;
      ivec4 light_type;
  };

  struct Material {
      vec4 albedo;
      float roughness;
      float reflectance;
      float ambient_ratio;
  };

  // light direction from point to light
  vec3 light_direction(Light light, vec3 position) {
      switch(light.light_type[0]) {
      case 0:
          return normalize(light.position.xyz - position);
      default:
          return light.position.xyz;
      }
  }

  vec3 irradiance(Light light, vec3 position, vec3 normal) {
      vec3 light_dir = light_direction(light, position);
      return light.color.xyz * clamp(dot(light_dir, normal), 0.0, 1.0);
  }

  vec3 diffuse_brdf(Material material) {
      return material.albedo.xyz * (1.0 - material.reflectance);
  }

  float microfacet_distribution(vec3 middle, vec3 normal, float alpha) {
      float dotNH = dot(normal, middle);
      float alpha2 = alpha * alpha;
      float sqrt_denom = 1.0 - dotNH * dotNH * (1.0 - alpha2);
      return alpha2 / (sqrt_denom * sqrt_denom);
  }

  float schlick_approxy(vec3 vec, vec3 normal, float k) {
      float dotNV = dot(normal, vec);
      return dotNV / (dotNV * (1.0 - k) + k);
  }

  float geometric_decay(vec3 light_dir, vec3 camera_dir, vec3 normal, float alpha) {
      float k = alpha / 2.0;
      return schlick_approxy(light_dir, normal, k) * schlick_approxy(camera_dir, normal, k);
  }

  vec3 fresnel(vec3 f0, vec3 middle, vec3 camera_dir) {
      float c = 1.0 - dot(middle, camera_dir);
      c = c * c * c * c * c;
      return f0 + (1.0 - f0) * c;
  }

  vec3 specular_brdf(Material material, vec3 camera_dir, vec3 light_dir, vec3 normal) {
      vec3 specular_color = material.albedo.xyz * material.reflectance;
      vec3 middle = normalize(camera_dir + light_dir);
      float alpha = material.roughness * material.roughness;
      float distribution = microfacet_distribution(middle, normal, alpha);
      float decay = geometric_decay(light_dir, camera_dir, normal, alpha);
      vec3 fresnel_color = fresnel(specular_color, middle, camera_dir);
      float dotCN = clamp(dot(camera_dir, normal), 0.0, 1.0);
      float dotLN = clamp(dot(light_dir, normal), 0.0, 1.0);
      float denom = 4.0 * dotCN * dotLN;
      if (denom < 1.0e-6) {
          return vec3(0.0, 0.0, 0.0);
      }
      return distribution * decay / denom * fresnel_color;
  }

  vec3 microfacet_color(vec3 position, vec3 normal, Light light, vec3 camera_dir, Material material) {
      vec3 light_dir = light_direction(light, position);
      vec3 irradiance = irradiance(light, position, normal);
      vec3 diffuse = diffuse_brdf(material);
      vec3 specular = specular_brdf(material, camera_dir, light_dir, normal);
      return (diffuse + specular) * irradiance;
  }

  vec3 ambient_correction(vec3 pre_color, Material material) {
      return pre_color * (1.0 - material.ambient_ratio)
          + material.albedo.xyz * material.ambient_ratio;
  }

  void main() {
      vec3 position = vertex_position;
      vec2 uv = vertex_uv;
      vec3 normal = normalize(vertex_normal);
      uv.y = 1.0 - uv.y;

  /* discard by texture */

      Material mat;
      mat.albedo = vec4(1.0);
      mat.roughness = 0.2;
      mat.reflectance = 0.1;
      mat.ambient_ratio = 0.04;

      Light light;
      light.position = vec4(camera_position, 1);
      light.color = vec4(0.93, 0.94, 0.96, 1.0);
      light.light_type = ivec4(0);

      vec3 camera_dir = normalize(camera_position - position);

      vec3 col = microfacet_color(position, normal, light, camera_dir, mat);
      col = ambient_correction(col, mat);

      color = vec4(col, 1.0);
  }
`;

class App {
  #fps = 1000 / 30;
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
    const resize_ob = new ResizeObserver(this.#resize.bind(this));
    resize_ob.observe(canvas);

    this.#gl =
      canvas.getContext("webgl2") || canvas.getContext("experimental-webgl");

    const prg = this.#createProgram(
      this.#createShader(vertexShader, "x-vertex"),
      this.#createShader(fragmentShader, "x-fragment")
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

  #createShader(shaderText, type) {
    let shader;

    switch (type) {
      case "x-vertex":
        shader = this.#gl.createShader(this.#gl.VERTEX_SHADER);
        break;
      case "x-fragment":
        shader = this.#gl.createShader(this.#gl.FRAGMENT_SHADER);
        break;
      default:
        return;
    }

    this.#gl.shaderSource(shader, shaderText);
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
      setTimeout(this.#render.bind(this), this.#fps);
      return;
    }

    let gl = this.#gl;
    let c = this.#canvas;

    gl.viewport(0, 0, gl.canvas.width, gl.canvas.height);

    gl.clear(gl.COLOR_BUFFER_BIT | gl.DEPTH_BUFFER_BIT);

    if (!this.#vBuffer) {
      gl.flush();
      this.#needsRender = false;
      setTimeout(this.#render.bind(this), this.#fps);
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
    setTimeout(this.#render.bind(this), this.#fps);
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

  #resize(_e) {
    var width = this.#canvas.clientWidth;
    var height = this.#canvas.clientHeight;
    if (this.#canvas.width != width || this.#canvas.height != height) {
      this.#canvas.width = width;
      this.#canvas.height = height;
    }
    this.#needsRender = true;
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

  let app;

  if (document.readyState !== "loading") {
    onLoad();
  } else {
    addEventListener("load", onLoad, false);
  }

  function onLoad() {
    let c = document.getElementById("canvas");

    app = new App(c);

    document.querySelector("input").addEventListener("change", fileRead);
  }

  function fileRead(e) {
    if (typeof e.target.value === "undefined") {
      console.warn("invalid input");
      return;
    }
    const solid = Truck.build_torus(0.5, e.target.value);
    let polygon = solid.to_polygon(0.01);
    app.draw(polygon);
  }
}

run();

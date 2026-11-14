// The interactive 3D logo (model.html): the model (logo3d.js) under a sky of
// stars, turned with the mouse or a finger (OrbitControls): drag to rotate,
// scroll or pinch to zoom, right-drag or two-finger drag to pan,
// double-click to reset the view. The view turns slowly by itself until the
// first interaction. Without WebGL, or if the model can't be loaded, the
// page shows the SVG logo and says why.

import * as THREE from 'three';
import { OrbitControls } from 'three/addons/controls/OrbitControls.js';
import { webglAvailable, createRenderer, createScene, loadLogo, mulberry32 } from './logo3d.js';

const FOV = 40;           // degrees (vertical)
const SKY_RADIUS = 400;   // the stars' distance: far beyond any zoom
const reduceMotion = window.matchMedia('(prefers-reduced-motion: reduce)').matches;

const canvas = document.querySelector('.viewer-3d');
const hint = document.querySelector('.viewer-hint');
const resetButton = document.querySelector('.viewer-reset');

// on a touch screen, the hint names the touch gestures
if (hint && window.matchMedia('(pointer: coarse)').matches) {
  hint.textContent = 'Drag to rotate · pinch to zoom · two-finger drag to pan';
}

function fallback(reason, error) {
  console.info(`PEPPER: the viewer shows the static logo (${reason})`, error ?? '');
  document.body.classList.add('viewer-static');
  const why = document.querySelector('.viewer-why');
  if (why) why.textContent = `The interactive model is not available: ${reason}.`;
}

if (!webglAvailable()) fallback('WebGL is not available in this browser');
else if (location.protocol === 'file:') fallback('the page is a local file; serve web/ over HTTP');
else start();

function start() {
  const renderer = createRenderer(canvas);
  const scene = createScene(renderer);
  scene.add(sky());

  const camera = new THREE.PerspectiveCamera(FOV, 1, 0.1, 2 * SKY_RADIUS);
  const controls = new OrbitControls(camera, canvas);
  controls.enableDamping = true;
  controls.dampingFactor = 0.08;
  controls.minDistance = 3;
  controls.maxDistance = 60;
  controls.autoRotate = !reduceMotion;
  controls.autoRotateSpeed = 0.6;  // a turn in about 100 s

  let spin = () => {};
  let fitted = false;

  // the distance that fits the logo's circle (10 units across) in the view
  function fitDistance() {
    const half = Math.tan(THREE.MathUtils.degToRad(FOV / 2));
    return 5.2 / (half * Math.min(1, camera.aspect));
  }
  function resetView() {
    camera.position.set(0, 0, fitDistance());
    controls.target.set(0, 0, 0);
    controls.update();
  }

  function resize() {
    const w = canvas.clientWidth, h = canvas.clientHeight;
    if (!w || !h) return;
    if (canvas.width !== Math.round(w * renderer.getPixelRatio()) ||
        canvas.height !== Math.round(h * renderer.getPixelRatio())) {
      renderer.setSize(w, h, false);
      camera.aspect = w / h;
      camera.updateProjectionMatrix();
      if (!fitted) { resetView(); fitted = true; }
    }
  }
  window.addEventListener('resize', resize);
  resize();

  // the first interaction stops the turning and fades the hint
  controls.addEventListener('start', () => {
    controls.autoRotate = false;
    hint?.classList.add('hidden');
  });
  canvas.addEventListener('dblclick', resetView);
  resetButton?.addEventListener('click', resetView);

  loadLogo((model, spinPlanets) => {
    if (!reduceMotion) spin = spinPlanets;
    scene.add(model);
    canvas.classList.add('ready');
    document.body.classList.add('viewer-ready');
    run();
  }, error => fallback('the model could not be loaded', error));

  const clock = new THREE.Clock();
  function frame() {
    const dt = Math.min(clock.getDelta(), 0.1);
    spin(dt);
    controls.update();
    resize();
    renderer.render(scene, camera);
  }
  // render only while the page is shown
  function run() {
    renderer.setAnimationLoop(document.hidden ? null : frame);
    clock.getDelta();  // no jump after a pause
  }
  document.addEventListener('visibilitychange', run);
}

// The sky: stars all around (they stay put while the view turns), brighter
// and denser along a band like the Milky Way. Points of three sizes, white
// with a few warm and blue ones, drawn as soft round dots of a fixed size
// on the screen.
function sky() {
  const rng = mulberry32(42);
  const sprite = dotTexture();
  const group = new THREE.Group();
  const layers = [
    { n: 5000, size: 1.6, opacity: 0.55, band: 0.35 },
    { n: 1500, size: 2.6, opacity: 0.8, band: 0.25 },
    { n: 250, size: 4.0, opacity: 1.0, band: 0.15 },
  ];
  const tints = [
    [1, 1, 1], [1, 0.85, 0.65], [0.7, 0.8, 1],  // white, warm, blue
  ];
  // the band's plane: tilted, as the galactic plane is to the ecliptic
  const tilt = new THREE.Quaternion().setFromEuler(new THREE.Euler(1.05, 0, 0.4));
  const v = new THREE.Vector3();
  for (const layer of layers) {
    const pos = new Float32Array(3 * layer.n), col = new Float32Array(3 * layer.n);
    for (let i = 0; i < layer.n; i++) {
      if (rng() < layer.band) {
        // near the band: a uniform longitude, a narrow (Gaussian) latitude
        const lon = 2 * Math.PI * rng();
        const lat = 0.12 * gaussian(rng);
        v.set(Math.cos(lat) * Math.cos(lon), Math.sin(lat), Math.cos(lat) * Math.sin(lon)).applyQuaternion(tilt);
      } else {
        // uniform over the sphere
        const z = 2 * rng() - 1, lon = 2 * Math.PI * rng(), r = Math.sqrt(1 - z * z);
        v.set(r * Math.cos(lon), z, r * Math.sin(lon));
      }
      v.multiplyScalar(SKY_RADIUS).toArray(pos, 3 * i);
      const u = rng(), tint = tints[u < 0.12 ? 1 : u < 0.24 ? 2 : 0];
      const b = 0.6 + 0.4 * rng();  // brightness
      col[3 * i] = tint[0] * b; col[3 * i + 1] = tint[1] * b; col[3 * i + 2] = tint[2] * b;
    }
    const g = new THREE.BufferGeometry();
    g.setAttribute('position', new THREE.BufferAttribute(pos, 3));
    g.setAttribute('color', new THREE.BufferAttribute(col, 3));
    group.add(new THREE.Points(g, new THREE.PointsMaterial({
      size: layer.size * Math.min(window.devicePixelRatio || 1, 2),
      sizeAttenuation: false, map: sprite, vertexColors: true,
      transparent: true, opacity: layer.opacity, depthWrite: false,
    })));
  }
  return group;
}

// a soft round dot, for the stars
function dotTexture() {
  const c = document.createElement('canvas');
  c.width = c.height = 64;
  const g = c.getContext('2d');
  const grad = g.createRadialGradient(32, 32, 0, 32, 32, 32);
  grad.addColorStop(0, 'rgba(255,255,255,1)');
  grad.addColorStop(0.35, 'rgba(255,255,255,0.8)');
  grad.addColorStop(1, 'rgba(255,255,255,0)');
  g.fillStyle = grad;
  g.fillRect(0, 0, 64, 64);
  const t = new THREE.CanvasTexture(c);
  t.colorSpace = THREE.SRGBColorSpace;
  return t;
}

// a standard normal deviate (Box-Muller)
function gaussian(rng) {
  return Math.sqrt(-2 * Math.log(1 - rng())) * Math.cos(2 * Math.PI * rng());
}

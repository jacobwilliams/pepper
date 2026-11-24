// The 3D logo (the model of media/make_logo_3d.py, pepper_logo.glb, and its
// variant with rings of power, media/make_rings_3d.py, pepper_rings.glb),
// shared by the hero (hero3d.js) and the interactive viewers (model3d.js):
// the renderer's settings, the lights, loading a model, and the spins of
// its planets or rings.

import * as THREE from 'three';
import { GLTFLoader } from 'three/addons/loaders/GLTFLoader.js';
import { RoomEnvironment } from 'three/addons/environments/RoomEnvironment.js';

// the spins, radians per second, by node name: the planets, and the rings
// (a ring's band, setting, stone and inlay are the nodes RING, RING_SET,
// RING_GEM and RING_INLAY: the spin of the name before the "_")
const SPIN = {
  MOON: 0.25, MARS: 0.5, EARTH: 0.6, VENUS: 0.2, MERCURY: 0.3,
  NEPTUNE: 0.7, URANUS: 0.6, SATURN: 0.8, JUPITER: 0.9,
  ONE: 0.7, FIRE: 1.1, AIR: -1.0, WATER: 1.2,
  DWARF1: -0.9, DWARF2: 0.95, MAN1: 1.3, MAN2: -1.25, MAN3: 1.4,
};
const spinOf = name => SPIN[name.split('_')[0]];

export function webglAvailable() {
  try {
    const c = document.createElement('canvas');
    return !!(c.getContext('webgl2') || c.getContext('webgl'));
  } catch {
    return false;
  }
}

// a renderer on the canvas, with the logo's color settings
export function createRenderer(canvas) {
  const renderer = new THREE.WebGLRenderer({ canvas, antialias: true, alpha: true });
  renderer.setPixelRatio(Math.min(window.devicePixelRatio || 1, 2));
  renderer.outputColorSpace = THREE.SRGBColorSpace;
  renderer.toneMapping = THREE.ACESFilmicToneMapping;
  return renderer;
}

// a scene lit for the logo: reflections for the chrome, a warm sun, a blue fill
export function createScene(renderer) {
  const scene = new THREE.Scene();
  const pmrem = new THREE.PMREMGenerator(renderer);
  scene.environment = pmrem.fromScene(new RoomEnvironment(), 0.04).texture;
  const sun = new THREE.DirectionalLight(0xfff4e0, 2.2);
  sun.position.set(-4, 5, 7);
  scene.add(sun);
  scene.add(new THREE.AmbientLight(0x8090ff, 0.25));
  return scene;
}

// Load the model at url (10 units across, centered at the origin; the logo
// by default). onLoad gets the model's root and a function spin(dt) that
// turns each planet or ring about its own (tilted) axis by dt seconds' worth.
export function loadLogo(onLoad, onError, url = 'pepper_logo.glb') {
  new GLTFLoader().load(url, gltf => {
    const spinning = [];
    gltf.scene.traverse(node => {
      const rate = spinOf(node.name);
      if (rate !== undefined) spinning.push([node, rate]);
    });
    const spin = dt => { for (const [node, rate] of spinning) node.rotateY(rate * dt); };
    onLoad(gltf.scene, spin);
  }, undefined, onError);
}

// a small deterministic random number generator (the same stars every time)
export function mulberry32(a) {
  return () => {
    a |= 0; a = (a + 0x6d2b79f5) | 0;
    let t = Math.imul(a ^ (a >>> 15), 1 | a);
    t = (t + Math.imul(t ^ (t >>> 7), 61 | t)) ^ t;
    return ((t ^ (t >>> 14)) >>> 0) / 4294967296;
  };
}

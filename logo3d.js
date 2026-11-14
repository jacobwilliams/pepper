// The 3D logo (the model of media/make_logo_3d.py, pepper_logo.glb), shared
// by the hero (hero3d.js) and the interactive viewer (model3d.js): the
// renderer's settings, the lights, loading the model, and the planets' spins.

import * as THREE from 'three';
import { GLTFLoader } from 'three/addons/loaders/GLTFLoader.js';
import { RoomEnvironment } from 'three/addons/environments/RoomEnvironment.js';

// the planets' spins, radians per second (the model's nodes have these names)
const SPIN = {
  MOON: 0.25, MARS: 0.5, EARTH: 0.6, VENUS: 0.2, MERCURY: 0.3,
  NEPTUNE: 0.7, URANUS: 0.6, SATURN: 0.8, JUPITER: 0.9,
};

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

// Load the model (10 units across, centered at the origin). onLoad gets the
// model's root and a function spin(dt) that turns each planet about its own
// (tilted) axis by dt seconds' worth.
export function loadLogo(onLoad, onError) {
  new GLTFLoader().load('pepper_logo.glb', gltf => {
    const planets = [];
    gltf.scene.traverse(node => {
      if (SPIN[node.name] !== undefined) planets.push(node);
    });
    const spin = dt => { for (const p of planets) p.rotateY(SPIN[p.name] * dt); };
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

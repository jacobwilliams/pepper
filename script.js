// PEPPER preview site: starfield, shooting stars, code tabs, scroll reveal
// and counting statistics. No dependencies.

(() => {
  const reduceMotion = window.matchMedia('(prefers-reduced-motion: reduce)').matches;

  // ---------------------------------------------------------------- stars
  const canvas = document.getElementById('stars');
  const ctx = canvas.getContext('2d');
  let width = 0, height = 0, dpr = 1;
  let stars = [];
  let meteors = [];
  const mouse = { x: 0, y: 0 };     // -1..1, for a gentle parallax
  const parallax = { x: 0, y: 0 };

  function resize() {
    dpr = Math.min(window.devicePixelRatio || 1, 2);
    width = window.innerWidth;
    height = window.innerHeight;
    canvas.width = width * dpr;
    canvas.height = height * dpr;
    ctx.setTransform(dpr, 0, 0, dpr, 0, 0);
    // about one star per 3500 px², in three depth layers
    const count = Math.round((width * height) / 3500);
    stars = Array.from({ length: count }, () => {
      const depth = Math.random() < 0.6 ? 1 : Math.random() < 0.7 ? 2 : 3;
      return {
        x: Math.random() * width,
        y: Math.random() * height,
        depth,
        r: 0.4 + depth * 0.45 * Math.random() + (depth === 3 ? 0.4 : 0),
        phase: Math.random() * Math.PI * 2,
        speed: 0.5 + Math.random() * 1.5,
        hue: Math.random() < 0.15 ? 30 : Math.random() < 0.3 ? 220 : 0,  // a few warm, a few blue
      };
    });
  }

  function spawnMeteor() {
    const angle = Math.PI * (0.15 + Math.random() * 0.15);   // down and to the left/right
    const dir = Math.random() < 0.5 ? 1 : -1;
    const speed = 9 + Math.random() * 6;
    meteors.push({
      x: Math.random() * width,
      y: Math.random() * height * 0.4,
      vx: Math.cos(angle) * speed * dir,
      vy: Math.sin(angle) * speed,
      life: 0,
      max: 50 + Math.random() * 30,
    });
  }

  let last = performance.now();
  function frame(now) {
    const dt = Math.min((now - last) / 16.67, 3);   // in 60 fps frames
    last = now;
    parallax.x += (mouse.x - parallax.x) * 0.04;
    parallax.y += (mouse.y - parallax.y) * 0.04;
    const scroll = window.scrollY;

    ctx.clearRect(0, 0, width, height);
    const t = now / 1000;
    for (const s of stars) {
      // slow drift, faster for nearer stars
      s.x -= 0.02 * s.depth * dt;
      if (s.x < -2) s.x = width + 2;
      const px = s.x + parallax.x * s.depth * 6;
      let py = (s.y - scroll * 0.03 * s.depth) % height;
      if (py < 0) py += height;
      py += parallax.y * s.depth * 6;
      const twinkle = 0.55 + 0.45 * Math.sin(t * s.speed + s.phase);
      const alpha = (0.35 + 0.2 * s.depth) * twinkle;
      ctx.beginPath();
      ctx.fillStyle = s.hue === 0
        ? `rgba(255,255,255,${alpha})`
        : `hsla(${s.hue},90%,80%,${alpha})`;
      ctx.arc(px, py, s.r, 0, Math.PI * 2);
      ctx.fill();
    }

    // shooting stars
    if (Math.random() < 0.006 * dt) spawnMeteor();
    meteors = meteors.filter(m => m.life < m.max);
    for (const m of meteors) {
      m.x += m.vx * dt;
      m.y += m.vy * dt;
      m.life += dt;
      const fade = 1 - m.life / m.max;
      const grad = ctx.createLinearGradient(m.x, m.y, m.x - m.vx * 8, m.y - m.vy * 8);
      grad.addColorStop(0, `rgba(255,236,200,${0.9 * fade})`);
      grad.addColorStop(1, 'rgba(255,236,200,0)');
      ctx.strokeStyle = grad;
      ctx.lineWidth = 1.6;
      ctx.beginPath();
      ctx.moveTo(m.x, m.y);
      ctx.lineTo(m.x - m.vx * 8, m.y - m.vy * 8);
      ctx.stroke();
    }

    if (!reduceMotion) requestAnimationFrame(frame);
  }

  resize();
  if (reduceMotion) {
    // still stars: one frame, and one after each resize
    frame(performance.now());
    window.addEventListener('resize', () => { resize(); frame(performance.now()); });
  } else {
    window.addEventListener('resize', resize);
    window.addEventListener('pointermove', e => {
      mouse.x = (e.clientX / width) * 2 - 1;
      mouse.y = (e.clientY / height) * 2 - 1;
    });
    requestAnimationFrame(frame);
  }

  // ---------------------------------------------------------------- tabs
  const tabs = document.querySelectorAll('.tabs button');
  const panes = document.querySelectorAll('.code[data-pane]');
  tabs.forEach(tab => tab.addEventListener('click', () => {
    tabs.forEach(t => t.setAttribute('aria-selected', String(t === tab)));
    panes.forEach(p => { p.hidden = p.dataset.pane !== tab.dataset.tab; });
  }));

  // ------------------------------------------------- reveal and count up
  function countUp(el) {
    const to = parseFloat(el.dataset.to);
    const decimals = parseInt(el.dataset.decimals || '0', 10);
    const prefix = el.dataset.prefix || '';
    const suffix = el.dataset.suffix || '';
    const format = v => prefix + v.toLocaleString('en-US', {
      minimumFractionDigits: decimals, maximumFractionDigits: decimals,
    }) + suffix;
    if (reduceMotion) { el.textContent = format(to); return; }
    const start = performance.now();
    const duration = 1400;
    function step(now) {
      const p = Math.min((now - start) / duration, 1);
      const eased = 1 - Math.pow(1 - p, 3);
      el.textContent = format(to * eased);
      if (p < 1) requestAnimationFrame(step);
    }
    requestAnimationFrame(step);
  }

  const observer = new IntersectionObserver(entries => {
    for (const entry of entries) {
      if (!entry.isIntersecting) continue;
      entry.target.classList.add('visible');
      entry.target.querySelectorAll('.num[data-to]').forEach(countUp);
      observer.unobserve(entry.target);
    }
  }, { threshold: 0.15 });
  document.querySelectorAll('.reveal').forEach(el => observer.observe(el));
})();

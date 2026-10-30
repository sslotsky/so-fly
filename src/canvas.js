const TAU = Math.PI * 2

const seeded = seed => () => {
  seed = (seed * 16807) % 2147483647
  return (seed - 1) / 2147483646
}

const makePads = (width, height) => {
  const rand = seeded(7)
  const pads = []
  for (let tries = 0; pads.length < 9 && tries < 400; tries++) {
    const pad = {
      x: 20 + rand() * (width - 40),
      y: 20 + rand() * (height - 40),
      r: 14 + rand() * 22,
      turn: rand() * TAU,
      drift: 0.4 + rand() * 0.6,
      flower: rand() < 0.3
    }
    const crowded = pads.some(p => Math.hypot(p.x - pad.x, p.y - pad.y) < p.r + pad.r + 24)
    if (!crowded) pads.push(pad)
  }
  return pads
}

const drawWater = (ctx, width, height, t) => {
  const water = ctx.createLinearGradient(0, 0, 0, height)
  water.addColorStop(0, '#16414a')
  water.addColorStop(1, '#0b2a30')
  ctx.fillStyle = water
  ctx.fillRect(0, 0, width, height)

  ctx.strokeStyle = 'rgba(190, 235, 230, 0.06)'
  ctx.lineWidth = 2
  for (let i = 0; i < 7; i++) {
    const y = ((i + 0.5) * height) / 7 + Math.sin(t / 2400 + i) * 6
    ctx.beginPath()
    for (let x = 0; x <= width; x += 20) {
      const wave = Math.sin(x / 70 + t / 1500 + i * 1.7) * 4
      x === 0 ? ctx.moveTo(x, y + wave) : ctx.lineTo(x, y + wave)
    }
    ctx.stroke()
  }
}

const drawPad = (ctx, pad, t) => {
  const sway = Math.sin(t / 3000 * pad.drift + pad.turn) * 0.08
  ctx.save()
  ctx.translate(pad.x, pad.y)
  ctx.rotate(pad.turn + sway)
  ctx.fillStyle = 'rgba(0, 0, 0, 0.18)'
  ctx.beginPath()
  ctx.ellipse(3, 4, pad.r, pad.r * 0.92, 0, 0, TAU)
  ctx.fill()
  ctx.fillStyle = '#3f7d3a'
  ctx.beginPath()
  ctx.moveTo(0, 0)
  ctx.arc(0, 0, pad.r, 0.25, TAU - 0.25)
  ctx.closePath()
  ctx.fill()
  ctx.strokeStyle = 'rgba(150, 200, 110, 0.35)'
  ctx.lineWidth = 1
  for (let i = 1; i < 6; i++) {
    const a = 0.25 + (i * (TAU - 0.5)) / 6
    ctx.beginPath()
    ctx.moveTo(0, 0)
    ctx.lineTo(Math.cos(a) * pad.r * 0.85, Math.sin(a) * pad.r * 0.85)
    ctx.stroke()
  }
  if (pad.flower) {
    ctx.translate(-pad.r * 0.35, -pad.r * 0.3)
    for (let i = 0; i < 8; i++) {
      ctx.fillStyle = i % 2 ? '#f6c9da' : '#ee9fbd'
      ctx.beginPath()
      ctx.ellipse(0, -4, 2.6, 5.5, 0, 0, TAU)
      ctx.fill()
      ctx.rotate(TAU / 8)
    }
    ctx.fillStyle = '#f4d35e'
    ctx.beginPath()
    ctx.arc(0, 0, 2.4, 0, TAU)
    ctx.fill()
  }
  ctx.restore()
}

const drawRipple = (ctx, ripple, t) => {
  const age = (t - ripple.born) / ripple.life
  ctx.strokeStyle = `rgba(200, 240, 235, ${0.35 * (1 - age)})`
  ctx.lineWidth = 1.5
  ctx.beginPath()
  ctx.ellipse(ripple.x, ripple.y, ripple.r * age, ripple.r * age * 0.6, 0, 0, TAU)
  ctx.stroke()
}

const drawFly = (ctx, fly, t, i) => {
  const jitterX = Math.sin(t / 37 + i * 3.1) * 0.8
  const jitterY = Math.cos(t / 29 + i * 1.7) * 0.8
  const flap = 0.55 + 0.45 * Math.abs(Math.sin(t / 18 + i))
  ctx.save()
  ctx.translate(fly.x + jitterX, fly.y + jitterY)
  ctx.fillStyle = 'rgba(0, 0, 0, 0.25)'
  ctx.beginPath()
  ctx.ellipse(3, 5, 2.5, 1.5, 0, 0, TAU)
  ctx.fill()
  ctx.fillStyle = `rgba(225, 245, 255, ${0.35 + 0.3 * flap})`
  for (const side of [-1, 1]) {
    ctx.beginPath()
    ctx.ellipse(side * 3, -2, 3.6 * flap, 2, side * 0.5, 0, TAU)
    ctx.fill()
  }
  ctx.fillStyle = '#1b1d22'
  ctx.beginPath()
  ctx.ellipse(0, 0, 2.3, 3.2, 0, 0, TAU)
  ctx.fill()
  ctx.fillStyle = '#8e2b2b'
  ctx.beginPath()
  ctx.arc(0, -2.6, 1.5, 0, TAU)
  ctx.fill()
  ctx.restore()
}

const drawFrog = (ctx, hero, heading, t) => {
  const s = hero.size * 10
  const breath = 1 + Math.sin(t / 420) * 0.025
  ctx.save()
  ctx.translate(hero.x, hero.y)
  ctx.rotate(heading + Math.PI / 2)
  ctx.scale(s / 10, s / 10)

  ctx.fillStyle = 'rgba(0, 0, 0, 0.28)'
  ctx.beginPath()
  ctx.ellipse(2, 4, 11, 13, 0, 0, TAU)
  ctx.fill()

  ctx.fillStyle = '#3e8f3a'
  for (const side of [-1, 1]) {
    ctx.beginPath()
    ctx.ellipse(side * 8, 7, 4, 8, side * -0.6, 0, TAU)
    ctx.fill()
    ctx.beginPath()
    ctx.ellipse(side * 7, -5, 2.4, 4.5, side * 0.7, 0, TAU)
    ctx.fill()
  }

  const body = ctx.createRadialGradient(-2, -3, 1, 0, 0, 12)
  body.addColorStop(0, '#8fd16a')
  body.addColorStop(1, '#4a9a3f')
  ctx.fillStyle = body
  ctx.beginPath()
  ctx.ellipse(0, 1, 8.5 * breath, 10 * breath, 0, 0, TAU)
  ctx.fill()

  ctx.fillStyle = 'rgba(40, 90, 35, 0.55)'
  for (const [x, y, r] of [[-3, 4, 1.6], [3, 6, 1.2], [1, 1, 1]]) {
    ctx.beginPath()
    ctx.arc(x, y, r, 0, TAU)
    ctx.fill()
  }

  const blink = (t % 4200) < 140
  for (const side of [-1, 1]) {
    ctx.fillStyle = '#6fbf55'
    ctx.beginPath()
    ctx.arc(side * 4.2, -7, 3.3, 0, TAU)
    ctx.fill()
    ctx.fillStyle = blink ? '#5aa847' : '#f4f1c9'
    ctx.beginPath()
    ctx.arc(side * 4.2, -7.3, 2.3, 0, TAU)
    ctx.fill()
    if (!blink) {
      ctx.fillStyle = '#16181c'
      ctx.beginPath()
      ctx.ellipse(side * 4.2, -7.5, 1.4, 0.9, 0, 0, TAU)
      ctx.fill()
    }
  }
  ctx.restore()
}

const mouth = (hero, heading) => ({
  x: hero.x + Math.cos(heading) * hero.size * 9,
  y: hero.y + Math.sin(heading) * hero.size * 9
})

const drawTongue = (ctx, from, lick, t) => {
  const age = (t - lick.born) / lick.life
  const reach = Math.sin(Math.min(1, age) * Math.PI)
  const x = from.x + (lick.x - from.x) * reach
  const y = from.y + (lick.y - from.y) * reach
  ctx.strokeStyle = '#e8688a'
  ctx.lineCap = 'round'
  ctx.lineWidth = 2.5
  ctx.beginPath()
  ctx.moveTo(from.x, from.y)
  ctx.lineTo(x, y)
  ctx.stroke()
  ctx.fillStyle = '#f08aa6'
  ctx.beginPath()
  ctx.arc(x, y, 2.8, 0, TAU)
  ctx.fill()
}

const drawHud = (ctx, gameData, width) => {
  const fullness = Math.min(1, (gameData.hero.size - 1) / 2)
  const left = width - 224
  ctx.fillStyle = 'rgba(6, 22, 26, 0.72)'
  ctx.beginPath()
  ctx.roundRect(left, 10, 214, 28, 14)
  ctx.fill()
  ctx.fillStyle = '#e9f5ec'
  ctx.font = '600 13px ui-rounded, "SF Pro Rounded", system-ui, sans-serif'
  ctx.textBaseline = 'middle'
  ctx.fillText(`Flies ${gameData.score}`, left + 14, 24)
  ctx.fillStyle = 'rgba(233, 245, 236, 0.6)'
  ctx.fillText('Belly', left + 82, 24)
  ctx.fillStyle = 'rgba(255, 255, 255, 0.12)'
  ctx.beginPath()
  ctx.roundRect(left + 124, 19, 78, 10, 5)
  ctx.fill()
  ctx.fillStyle = fullness > 0.15 ? '#8fd16a' : '#e0b04f'
  ctx.beginPath()
  ctx.roundRect(left + 124, 19, Math.max(10, 78 * fullness), 10, 5)
  ctx.fill()
}

class GameCanvas extends HTMLElement {
  static get observedAttributes() {
    return ['width', 'height']
  }

  constructor() {
    super()
    const shadow = this.attachShadow({ mode: 'open' })
    const style = document.createElement('style')
    style.textContent = ':host { display: block; } canvas { display: block; width: 100%; height: auto; border-radius: 18px; }'
    this.canvas = document.createElement('canvas')
    shadow.append(style, this.canvas)
    this.ripples = []
    this.licks = []
    this.heading = -Math.PI / 2
    this.previous = null
    this.resize()
  }

  attributeChangedCallback() {
    this.resize()
  }

  width() {
    return Number(this.getAttribute('width')) || 600
  }

  height() {
    return Number(this.getAttribute('height')) || 400
  }

  resize() {
    const ratio = window.devicePixelRatio || 1
    this.canvas.width = this.width() * ratio
    this.canvas.height = this.height() * ratio
    this.canvas.style.aspectRatio = `${this.width()} / ${this.height()}`
    this.pads = makePads(this.width(), this.height())
  }

  track(gameData, t) {
    const previous = this.previous
    this.previous = gameData
    if (!previous) return

    const dx = gameData.hero.x - previous.hero.x
    const dy = gameData.hero.y - previous.hero.y
    const speed = Math.hypot(dx, dy)
    if (speed > 0.05) this.heading = Math.atan2(dy, dx)
    if (speed > 0.4 && t - (this.lastWake || 0) > 160) {
      this.lastWake = t
      this.ripples.push({ x: previous.hero.x, y: previous.hero.y, r: 10 + gameData.hero.size * 10, born: t, life: 900 })
    }

    if (gameData.score > previous.score) {
      for (const fly of previous.flies) {
        const stillHere = gameData.flies.some(f => Math.abs(f.x - fly.x) < 1 && Math.abs(f.y - fly.y) < 1)
        if (!stillHere) {
          this.licks.push({ x: fly.x, y: fly.y, born: t, life: 180 })
          this.ripples.push({ x: fly.x, y: fly.y, r: 18, born: t, life: 600 })
        }
      }
    }

    if (Math.random() < 0.004) {
      this.ripples.push({ x: Math.random() * this.width(), y: Math.random() * this.height(), r: 26, born: t, life: 1800 })
    }
    this.ripples = this.ripples.filter(r => t - r.born < r.life)
    this.licks = this.licks.filter(l => t - l.born < l.life)
  }

  flick() {
    const hero = this.previous && this.previous.hero
    if (!hero) return
    this.licks.push({ reach: hero.size * 20, born: performance.now(), life: 220 })
  }

  tongueTip(hero, lick) {
    if (lick.reach === undefined) return lick
    return {
      ...lick,
      x: hero.x + Math.cos(this.heading) * lick.reach,
      y: hero.y + Math.sin(this.heading) * lick.reach
    }
  }

  draw(gameData) {
    const t = performance.now()
    this.track(gameData, t)
    const width = this.width()
    const height = this.height()
    const ctx = this.canvas.getContext('2d')
    const ratio = this.canvas.width / width
    ctx.setTransform(ratio, 0, 0, ratio, 0, 0)

    drawWater(ctx, width, height, t)
    this.ripples.forEach(r => drawRipple(ctx, r, t))
    this.pads.forEach(p => drawPad(ctx, p, t))
    drawFrog(ctx, gameData.hero, this.heading, t)
    this.licks.forEach(l => drawTongue(ctx, mouth(gameData.hero, this.heading), this.tongueTip(gameData.hero, l), t))
    gameData.flies.forEach((fly, i) => drawFly(ctx, fly, t, i))
    drawHud(ctx, gameData, width)
  }
}

customElements.define('game-canvas', GameCanvas)

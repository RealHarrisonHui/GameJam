shader_type canvas_item;
// The light in the vault's mouth. Decorative, so it runs on the presentation clock
// that effects.gd pauses explicitly, not on TIME.

uniform float pclock = 0.0;
uniform float open = 0.0;

void fragment() {
	vec2 p = UV - vec2(0.5);
	float r = length(p) * 2.0;
	float a = atan(p.y, p.x);
	float rays = 0.5 + 0.5 * sin(a * 9.0 + pclock * 0.5) * sin(a * 5.0 - pclock * 0.33);
	float core = smoothstep(0.5, 0.0, r);
	float halo = smoothstep(1.0, 0.25, r) * (0.34 + 0.5 * rays);
	float glow = core * 0.62 + halo * (0.62 + open * 0.5);
	vec3 col = mix(vec3(1.2, 0.72, 0.3), vec3(1.5, 1.2, 0.78), core);
	COLOR = vec4(col * (0.9 + open * 0.35), clamp(glow, 0.0, 1.0));
}

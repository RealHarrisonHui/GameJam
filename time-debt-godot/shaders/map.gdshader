shader_type canvas_item;
// The level's floor and walls. The world carries the temporal language here rather
// than in a full-screen pass: the front refracts this sprite only, so hazard and
// player silhouettes are never distorted.
#include "res://shaders/temporal.gdshaderinc"

varying vec2 world;

void vertex() {
	world = (MODEL_MATRIX * vec4(VERTEX, 0.0, 1.0)).xy;
}

void fragment() {
	float front = td_front(world);
	vec2 uv = UV;
	if (front > 0.001) {
		vec2 dir = normalize(world - td_wash_center + vec2(0.001));
		uv += dir * front * 7.0 / vec2(1280.0, 720.0);
	}
	vec4 tex = texture(TEXTURE, uv);
	vec3 c = tex.rgb;
	float on_floor = smoothstep(0.6, 0.9, tex.a);
	vec3 graded = td_temporal(c, world);
	if (td_detail > 0.5) {
		float tone_mix = smoothstep(td_wash_radius, td_wash_radius - 26.0, distance(world, td_wash_center));
		float tone = td_tone_from == td_tone_to ? td_tone_to : (tone_mix > 0.5 ? td_tone_to : td_tone_from);
		if (tone > 0.5 && tone < 1.5) {
			// A blueprint's hairline grid.
			vec2 g = abs(fract(world / 20.0) - 0.5) * 20.0;
			float hair = smoothstep(9.45, 9.95, max(g.x, g.y));
			graded += vec3(0.05, 0.12, 0.2) * hair * mix(0.22, 1.0, on_floor);
		} else if (tone > 1.5 && tone < 2.5) {
			// Ledger rules that race past with the hazard clock.
			float rule = smoothstep(0.93, 1.0, sin(world.y * 0.24 + td_hazard_time * 9.0));
			graded += vec3(0.07, 0.006, 0.004) * rule * on_floor;
		}
	}
	graded += td_edge_tint(td_tone_to) * front * 0.55;
	COLOR = vec4(graded, 1.0);
}

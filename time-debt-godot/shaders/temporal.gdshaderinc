// The world's mode language, shared by everything that sits in the world's "paper":
// floors, walls, gears and dial. Hazards and the player do not use this, so they
// stay crisp and read the same in every mode. All of it is driven by global uniforms
// set in effects.gd; nothing here reads TIME.

global uniform float td_tone_from;     // 0 normal, 1 frozen, 2 repaying, 3 settled, 4 foreclosed
global uniform float td_tone_to;
global uniform vec2 td_wash_center;    // world pixels
global uniform float td_wash_radius;   // inside this radius the world is td_tone_to
global uniform float td_wash_edge;     // 0..1 brightness of the travelling front
global uniform float td_detail;        // 1 in full effects, 0 in reduced
global uniform float td_hazard_time;   // hazard clock in seconds; stops and races with it

float td_luma(vec3 c) {
	return dot(c, vec3(0.2126, 0.7152, 0.0722));
}

// Colours are written in linear light because HDR 2D renders in linear.
vec3 td_grade(vec3 c, float tone) {
	if (tone < 0.5) {
		return c;
	}
	float l = td_luma(c);
	if (tone < 1.5) {
		// Cyanotype: prussian shadows, paper-pale highlights.
		float t = clamp(pow(l, 0.62) * 1.25, 0.0, 1.0);
		vec3 ramp = mix(vec3(0.006, 0.034, 0.085), vec3(0.17, 0.42, 0.62), smoothstep(0.0, 0.6, t));
		ramp = mix(ramp, vec3(0.78, 0.92, 0.98), smoothstep(0.6, 1.0, t));
		return ramp;
	}
	if (tone < 2.5) {
		// Ledger ink: everything sinks toward red, highlights stay warm paper.
		float t = clamp(pow(l, 0.68) * 1.2, 0.0, 1.0);
		vec3 ramp = mix(vec3(0.012, 0.002, 0.003), vec3(0.50, 0.035, 0.03), smoothstep(0.0, 0.6, t));
		ramp = mix(ramp, vec3(0.95, 0.55, 0.32), smoothstep(0.6, 1.0, t));
		return mix(c * vec3(0.9, 0.5, 0.45), ramp, 0.82);
	}
	if (tone < 3.5) {
		// Settled: gilded, warmer and brighter than normal.
		return mix(c * vec3(1.28, 1.14, 0.86), vec3(1.0, 0.76, 0.36) * pow(l, 0.72) * 1.3, 0.55);
	}
	// Foreclosed: drained of colour.
	return vec3(l) * vec3(0.62, 0.6, 0.6);
}

// Inside the travelling front the world is td_tone_to, outside it td_tone_from.
vec3 td_temporal(vec3 c, vec2 world) {
	if (td_tone_from == td_tone_to) {
		return td_grade(c, td_tone_to);
	}
	float k = smoothstep(td_wash_radius, td_wash_radius - 26.0, distance(world, td_wash_center));
	return mix(td_grade(c, td_tone_from), td_grade(c, td_tone_to), k);
}

vec3 td_edge_tint(float tone) {
	if (tone < 0.5) { return vec3(0.9, 0.82, 0.62); }
	if (tone < 1.5) { return vec3(0.55, 0.95, 1.5); }
	if (tone < 2.5) { return vec3(1.6, 0.32, 0.2); }
	if (tone < 3.5) { return vec3(1.7, 1.25, 0.55); }
	return vec3(0.6);
}

// 0..1 band around the travelling front.
float td_front(vec2 world) {
	if (td_wash_edge <= 0.0 || td_tone_from == td_tone_to) {
		return 0.0;
	}
	float d = (distance(world, td_wash_center) - td_wash_radius) / 9.0;
	return exp(-d * d) * td_wash_edge;
}

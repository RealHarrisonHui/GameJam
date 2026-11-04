shader_type canvas_item;
// Gears, dial and anything else mounted in the wall: same grade as the map.
#include "res://shaders/temporal.gdshaderinc"

varying vec2 world;

void vertex() {
	world = (MODEL_MATRIX * vec4(VERTEX, 0.0, 1.0)).xy;
}

void fragment() {
	vec4 c = texture(TEXTURE, UV) * COLOR;
	COLOR = vec4(td_temporal(c.rgb, world), c.a);
}

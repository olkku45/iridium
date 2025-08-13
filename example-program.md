```
// 2D physics sim, syntax demo

import std::random;

struct Ball {
    // considering the ability to type Ball.position.x/y/z if vec3 for example
	// yes position is vec2, because why not :D
    position: vec2,
    velocity: vec2,
    radius: f32,
    mass: f32
}

struct CollisionData {
    impulse: vec2,
    penetration: f32
}

// same as impl in Rust
define Ball {
    fn new(pos: vec2, vel: vec2, radius: f32, mass: f32): Ball {
        Ball { 
            position: pos, 
            velocity: vel, 
            radius, 
            mass 
        }
    }
}

fn::alloc create_balls(count: u32): array<Ball> {
    let mut balls = array::new();
    
    for (i in 0..count) {
	    // you declare tuples and vectors in the same way, but
		// you can't operate on tuples
        let pos: tup<f32> = (
            random::frange(0.0, 800.0),
            random::frange(0.0, 600.0)
        );
        let vel: vec2 = (
            random::frange(-100.0, 100.0),
            random::frange(-100.0, 100.0)
        );
        
        let ball = Ball::new(pos, vel, 10.0, 1.0);
        balls.add(ball);
    }
    
    return balls;
}

fn update_positions(balls: &mut [Ball], dt: f32) {
    // parallel update, no allocation needed
    parallel for (ball in balls) {
        ball.position += ball.velocity * dt;
        
        // bounce off walls using built-in vector operations
        if ball.position[0] <= ball.radius || ball.position[0] >= 800.0 - ball.radius {
            ball.velocity.x *= -0.8;
        }
        if ball.position[1] <= ball.radius || ball.position[1] >= 600.0 - ball.radius {
            ball.velocity[1] *= -0.8;
        }
        
        // clamp position to bounds
        ball.position[0] = clamp(ball.position[0], ball.radius, 800.0 - ball.radius);
        ball.position[1] = clamp(ball.position[1], ball.radius, 600.0 - ball.radius);
    }
}

fn::alloc check_collisions(balls: &mut [Ball]) {
    // pre-allocate collision data - worst case is every ball collides
    palloc(balls, CollisionData.size());
    
    parallel for (i in 0..balls.len()) {
        let ball_a = &balls[i];
        
        for (j in (i + 1)..balls.len()) {
            let ball_b = &balls[j];
            
            let delta = ball_a.position - ball_b.position;
            let distance = delta.len();
            let min_distance = ball_a.radius + ball_b.radius;
            
            if (distance < min_distance) {
                // use pre-allocated collision data
                let mut collision = getalloc();
                
                let penetration = min_distance - distance;
                let normal = delta.norm();
                
                // elastic collision
                let relative_velocity = ball_a.velocity - ball_b.velocity;
                let velocity_along_normal = relative_velocity.dot(normal);
                
                if velocity_along_normal > 0.0 {
                    continue;
                }
                
                let impulse_scalar = -2.0 * velocity_along_normal / (ball_a.mass + ball_b.mass);
                collision.impulse = normal * impulse_scalar;
                collision.penetration = penetration;
                
                // apply impulse (this would need thread-safe access in real implementation)
                apply_collision_response(balls, i, j, &collision);
            }
        }
    }
    // clear pre-allocated memory
    pclear();
}

fn apply_collision_response(balls: &mut [Ball], i: usize, j: usize, collision: &CollisionData) {
    let mass_a = balls[i].mass;
    let mass_b = balls[j].mass;
    
    balls[i].velocity += collision.impulse / mass_a;
    balls[j].velocity -= collision.impulse / mass_b;
    
    // separate balls to prevent overlap
    let separation = collision.impulse.norm() * collision.penetration * 0.5;
    balls[i].position += separation;
    balls[j].position -= separation;
}

fn apply_gravity(balls: &mut [Ball], gravity: f32, dt: f32) {
    let gravity_vec: vec2 = (0.0, gravity);
    
    parallel (for ball in balls) {
        ball.velocity += gravity_vec * dt;
    }
}

fn apply_wind_force(balls: &mut [Ball], wind: vec2, dt: f32) {
    parallel (for ball in balls) {
        // wind resistance based on velocity
        let air_resistance = ball.velocity * -0.01;
        let total_force = wind + air_resistance;
        ball.velocity += total_force * dt;
    }
}

fn rotate_system(balls: &mut [Ball], center: vec2, rotation: f32) {
    let rotation_matrix = mat2::rotation(rotation);
    
    parallel (for ball in balls) {
        let offset = ball.position - center;
        let rotated_offset = rotation_matrix * offset;
        ball.position = center + rotated_offset;
        
        // rotate velocity as well
        ball.velocity = rotation_matrix * ball.velocity;
    }
}

fn main() {
    let mut balls = create_balls(50);
    let dt = 1.0 / 60.0;  // 60 FPS
    let gravity = 500.0;
    let wind: vec2 = (50.0, 0.0);
    let screen_center: vec2 = (400.0, 300.0);
    
    // main loop
    for (frame in 0..1000) {
        update_positions(&mut balls, dt);
        apply_gravity(&mut balls, gravity, dt);
        apply_wind_force(&mut balls, wind, dt);
        check_collisions(&mut balls);
        
        // occasionally rotate the entire system
        if (frame % 120 == 0) {
            rotate_system(&mut balls, screen_center, 0.1);
        }
        
        if (frame % 60 == 0) {
            println("Frame: ${frame}, Active balls: ${balls.len()}");
            
            let total_momentum = balls.iter()
                .map(ball: ball.velocity * ball.mass)
                .sum<vec2>();
            println("Total momentum: (${total_momentum[0}, ${total_momentum[1]})");
        }
    }
    
}
#include "x86/avx.h"
#include "x86/avx2.h"
#include "x86/svml.h"

#define UP8 simde_mm256_set_ps(7,6,5,4,3,2,1,0)
#define PI 3.14159265358979323846
#define PI8 simde_mm256_set1_ps(PI)
#define PERIOD8 simde_mm256_set1_ps(2 * PI)

#define SIN_LOOKUP_SIZE 256

struct Coefficients {
  float taylor[9];
  float signum; 
  float abs;
};

float sine_table[SIN_LOOKUP_SIZE];

void setup_simd_routines(){
  for (int i = 0; i < SIN_LOOKUP_SIZE; i++){
    sine_table[i] = sin((i * 2 * PI) / (SIN_LOOKUP_SIZE - 1)); 
    // printf("S: %f", sine_table[i]);
  }

}


void print_simde__m256(simde__m256 vec) {
    float* values = (float*)&vec; // Cast to float pointer

    // Print each element of the vector
    printf("simde__m256 values: ");
    for (int i = 0; i < 8; ++i) {
        printf("%f ", values[i]);
    }
    printf("\n");
}

simde__m256 simde_signum(simde__m256 x) {
    simde__m256 zero = simde_mm256_setzero_ps();
    simde__m256 positive_mask = simde_mm256_cmp_ps(x, zero , SIMDE_CMP_GT_OQ);  // x > 0 → mask for +1.0
    simde__m256 negative_mask = simde_mm256_cmp_ps(x, zero, SIMDE_CMP_LT_OQ);  // x < 0 → mask for -1.0

    // Use masks to generate the result
    simde__m256 pos_part = simde_mm256_and_ps(positive_mask, simde_mm256_set1_ps(1.0f));  // +1.0 where x > 0
    simde__m256 neg_part = simde_mm256_and_ps(negative_mask, simde_mm256_set1_ps(-1.0f)); // -1.0 where x < 0

    return simde_mm256_add_ps(pos_part, neg_part);  // Combine the positive and negative parts
}

simde__m256 simde_abs(simde__m256 x) {
    simde__m256 sign_mask = simde_mm256_set1_ps(-0.0f);  // Sign bit mask (-0.0f is 0x80000000)
    return simde_mm256_andnot_ps(sign_mask, x);           // Zero out the sign bit
}

// Move all xs into -1 ... 1 
simde__m256 modulo1(simde__m256 x){
    simde__m256 modulo  = simde_mm256_set1_ps(2.0);
    simde__m256 quotient = _mm256_div_ps(x, modulo);
    simde__m256 floored = _mm256_floor_ps(quotient);
    simde__m256 product = _mm256_mul_ps(floored, modulo);
    simde__m256 x2 = _mm256_sub_ps(x, product);
    return _mm256_sub_ps(x2, simde_mm256_set1_ps(1.0));
}

void computeF(float* buffer, int samples, Coefficients* coefficients) {

  int i = 0;
  for (; i < samples - 7; i = i + 8) {
    simde__m256 is = simde_mm256_add_ps(UP8, simde_mm256_set1_ps(i));
    simde__m256 x = modulo1(is);
    simde__m256 x2 = simde_mm256_mul_ps(x, x);
    simde__m256 x3 = simde_mm256_mul_ps(x2, x);
    simde__m256 x4 = simde_mm256_mul_ps(x3, x);
    simde__m256 x5 = simde_mm256_mul_ps(x4, x); 
    simde__m256 x6 = simde_mm256_mul_ps(x5, x); 
    simde__m256 x7 = simde_mm256_mul_ps(x6, x); 
    simde__m256 x8 = simde_mm256_mul_ps(x7, x); 
    simde__m256 x9 = simde_mm256_mul_ps(x8, x); 
    
    simde__m256 result = simde_mm256_set1_ps(*coefficients.taylor[0]);
    result = simde_mm256_fmadd_ps(simde_mm256_set1_ps(*coefficients.taylor[1]), x, result);
    result = simde_mm256_fmadd_ps(simde_mm256_set1_ps(*coefficients.taylor[2]), x2, result);
    result = simde_mm256_fmadd_ps(simde_mm256_set1_ps(*coefficients.taylor[3]), x3, result);
    result = simde_mm256_fmadd_ps(simde_mm256_set1_ps(*coefficients.taylor[4]), x4, result);
    result = simde_mm256_fmadd_ps(simde_mm256_set1_ps(*coefficients.taylor[5]), x5, result);
    result = simde_mm256_fmadd_ps(simde_mm256_set1_ps(*coefficients.taylor[6]), x6, result);
    result = simde_mm256_fmadd_ps(simde_mm256_set1_ps(*coefficients.taylor[7]), x7, result);
    result = simde_mm256_fmadd_ps(simde_mm256_set1_ps(*coefficients.taylor[8]), x8, result);
    result = simde_mm256_fmadd_ps(simde_mm256_set1_ps(*coefficients.taylor[9]), x9, result);

    result = simde_mm256_fmadd_ps(simde_mm256_set1_ps(*coefficients.signum), simde_signum(x), result);
    result = simde_mm256_fmadd_ps(simde_mm256_set1_ps(*coefficients.abs), simde_abs(x), result);
    
    simde__m256* vec8 = (simde__m256*)(buffer + i);
    *vec8 = result;
  }
  for (;i < samples ; i = i + 1) {
    *(buffer + i) = amplitude * sin(2 * PI * hz * i);
  }
} 








// simde__m256 simde_sin_taylor(simde__m256 input) {

//     // Need to clamp input between -2pi ... 2 pi
//     simde__m256 quotient = _mm256_div_ps(input, PERIOD8);               // a / b
//     simde__m256 floored = _mm256_floor_ps(quotient);         // floor(a / b)
//     simde__m256 product = _mm256_mul_ps(floored, PERIOD8);         // floor(a / b) * b
//     simde__m256 x = _mm256_sub_ps(input, product);        // a - (floor(a / b) * b)
   
//     x = _mm256_sub_ps(x, PI8); 
//     x = _mm256_mul_ps(x, simde_mm256_set1_ps(-1)); 
  
//     // Constants for the approximation
//     const simde__m256 coeff1 = simde_mm256_set1_ps(1.0f / 6.0f);   // 1/6
//     const simde__m256 coeff2 = simde_mm256_set1_ps(1.0f / 120.0f); // 1/120
//     const simde__m256 coeff3 = simde_mm256_set1_ps(1.0f / 5040.0f); // 1/5040
//     const simde__m256 coeff4 = simde_mm256_set1_ps(1.0f / 362880.0f); 
//     const simde__m256 coeff5 = simde_mm256_set1_ps(1.0f / 39916800.0f); 

//     // Compute x^2, x^3, and x^5
//     simde__m256 x2 = simde_mm256_mul_ps(x, x);
//     simde__m256 x3 = simde_mm256_mul_ps(x2, x);
//     simde__m256 x5 = simde_mm256_mul_ps(x2, x3); 
//     simde__m256 x7 = simde_mm256_mul_ps(x5, x2); 
//     simde__m256 x9 = simde_mm256_mul_ps(x7, x2); 
//     simde__m256 x11 = simde_mm256_mul_ps(x9, x2); 

//     // Compute the approximation: x - (x^3 / 6) + (x^5 / 120)
//     simde__m256 term1 = x;
//     simde__m256 term2 = simde_mm256_mul_ps(x3, coeff1);
//     simde__m256 term3 = simde_mm256_mul_ps(x5, coeff2);
//     simde__m256 term4 = simde_mm256_mul_ps(x7, coeff3);
//     simde__m256 term5 = simde_mm256_mul_ps(x7, coeff4);
//     simde__m256 term6 = simde_mm256_mul_ps(x9, coeff5);

//     // Combine the terms: term1 - term2 + term3
//     return simde_mm256_sub_ps(simde_mm256_add_ps(simde_mm256_sub_ps(simde_mm256_add_ps(simde_mm256_sub_ps(term1, term2), term3), term4), term5), term6);
// }

// simde__m256 simde_sin_taylor(simde__m256 input) {

//     // Need to clamp input between -2pi ... 2 pi
//     simde__m256 quotient = _mm256_div_ps(input, PERIOD8);               // a / b
//     simde__m256 floored = _mm256_floor_ps(quotient);         // floor(a / b)
//     simde__m256 product = _mm256_mul_ps(floored, PERIOD8);         // floor(a / b) * b
//     simde__m256 x = _mm256_sub_ps(input, product);        // a - (floor(a / b) * b)
   
//     x = _mm256_sub_ps(x, PI8); 
//     x = _mm256_mul_ps(x, simde_mm256_set1_ps(-1)); 
  
//     // Constants for the approximation
//     const simde__m256 coeff1 = simde_mm256_set1_ps(1.0f / 6.0f);   // 1/6
//     const simde__m256 coeff2 = simde_mm256_set1_ps(1.0f / 120.0f); // 1/120
//     const simde__m256 coeff3 = simde_mm256_set1_ps(1.0f / 5040.0f); // 1/5040
//     const simde__m256 coeff4 = simde_mm256_set1_ps(1.0f / 362880.0f); 
//     const simde__m256 coeff5 = simde_mm256_set1_ps(1.0f / 39916800.0f); 

//     // Compute x^2, x^3, and x^5
//     simde__m256 x2 = simde_mm256_mul_ps(x, x);
//     simde__m256 x3 = simde_mm256_mul_ps(x2, x);
//     simde__m256 x5 = simde_mm256_mul_ps(x2, x3); 
//     simde__m256 x7 = simde_mm256_mul_ps(x5, x2); 
//     simde__m256 x9 = simde_mm256_mul_ps(x7, x2); 
//     simde__m256 x11 = simde_mm256_mul_ps(x9, x2); 

//     // Compute the approximation: x - (x^3 / 6) + (x^5 / 120)
//     simde__m256 term1 = x;
//     simde__m256 term2 = simde_mm256_mul_ps(x3, coeff1);
//     simde__m256 term3 = simde_mm256_mul_ps(x5, coeff2);
//     simde__m256 term4 = simde_mm256_mul_ps(x7, coeff3);
//     simde__m256 term5 = simde_mm256_mul_ps(x7, coeff4);
//     simde__m256 term6 = simde_mm256_mul_ps(x9, coeff5);

//     // Combine the terms: term1 - term2 + term3
//     return simde_mm256_sub_ps(simde_mm256_add_ps(simde_mm256_sub_ps(simde_mm256_add_ps(simde_mm256_sub_ps(term1, term2), term3), term4), term5), term6);
// }


// simde__m256 simde_sin_mem(simde__m256 input) {

//     // Need to clamp input between -2pi ... 2 pi
//     simde__m256 quotient = _mm256_div_ps(input, PERIOD8);               // a / b
//     simde__m256 floored = _mm256_floor_ps(quotient);         // floor(a / b)
//     simde__m256 product = _mm256_mul_ps(floored, PERIOD8);         // floor(a / b) * b
//     simde__m256 x = _mm256_sub_ps(input, product);        // a - (floor(a / b) * b)

  
//     simde__m256 rounded = simde_mm256_round_ps(simde_mm256_mul_ps(x, simde_mm256_set1_ps(SIN_LOOKUP_SIZE / (2 * PI))), SIMDE_MM_ROUND_NEAREST);
//     simde__m256i indices = simde_mm256_cvtps_epi32(rounded);
//     simde__m256 gathered = simde_mm256_i32gather_ps(sine_table, indices, 4);

//     return gathered;
// }

// void simple_sin(float* buffer, int samples, float amplitude, float hz) {
//   // float* buffer = (float*)aligned_alloc(32, 100 * sizeof(float));
//   // int samples = 100;
  
//   int i = 0;
//   for (; i < samples - 7; i = i + 8) {
//     simde__m256 xs = simde_mm256_add_ps(UP8, simde_mm256_set1_ps(i));
//     simde__m256 timings = simde_mm256_mul_ps(xs, simde_mm256_set1_ps(2 * PI * hz));
//     simde__m256 sinValues = simde_sin(timings);
//     simde__m256 finalValues = simde_mm256_mul_ps(sinValues, simde_mm256_set1_ps(amplitude));

//     simde__m256* vec8 = (simde__m256*)(buffer + i);
//     *vec8 = finalValues;
//   }
//   for (;i < samples ; i = i + 1) {
//     *(buffer + i) = amplitude * sin(2 * PI * hz * i);
//   }
// } 



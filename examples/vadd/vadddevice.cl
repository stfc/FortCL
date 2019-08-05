
// Vector addition Z = X + Y
__kernel void vadd(__global const float *x, 
                         __global const float *y, 
                         __global float *restrict z)
{
    int index = get_global_id(0); // Get work item
    z[index] = x[index] + y[index];
}



// Vector addition Z = X + Y
__kernel void vadd(__global float *x,  __global float *y,  __global float *z)
{
    int index = get_global_id(0); // Get work item
    z[index] = x[index] + y[index];
}


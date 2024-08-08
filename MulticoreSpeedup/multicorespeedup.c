/***************************************************************************/
/* Template for Asssignment 1 **********************************************/
/* Reza Malik                 **********************************************/
/***************************************************************************/

/***************************************************************************/
/* Includes ****************************************************************/
/***************************************************************************/

#include<stdio.h>
#include<stdlib.h>
#include<string.h>
#include<errno.h>
#include<math.h>

/***************************************************************************/
/* Defines *****************************************************************/
/***************************************************************************/

#define MAX_CORE_DOUBLINGS 20
#define MAX_NODES 1<< (MAX_CORE_DOUBLINGS/2) // 2^10
#define MAX_CORES_PER_NODE 1<< (MAX_CORE_DOUBLINGS/2) // 2^10
#define MAX_CORES_IN_SYSTEM (MAX_NODES * MAX_CORES_PER_NODE)
#define MAX_F 5
#define PERF(r) sqrt((double)r)

/***************************************************************************/
/* Global Vars *************************************************************/
/***************************************************************************/

double f[MAX_F] = {0.99999999, 0.9999, 0.99, 0.9, 0.5};

/***************************************************************************/
/* Function Decs ***********************************************************/
/***************************************************************************/

void compute_speedups();

/***************************************************************************/
/* Function: Main **********************************************************/
/***************************************************************************/


int main(int argc, char** argv)
{
  if (argc != 2){
      printf("ERROR USAGE: ./<outfile> <f_index>\n");
      exit(1);
  }
  compute_speedups(atoi(argv[1]));
  return 0;
}

/***************************************************************************/
/* Function: compute_speedups **********************************************/
/***************************************************************************/

void compute_speedups(int f_index)
{
  int r_index=0, core_index=0, node_index=0;
  double speedup_asymmetric = 0.0;
  double speedup_dynamic = 0.0;

  speedup_asymmetric = speedup_dynamic;
  speedup_dynamic = speedup_asymmetric;
  
    for(node_index = 1; node_index <= MAX_NODES; node_index = node_index << 2){
  //       2nd loop: node_index: 1 to MAX_NODES by increments of 4x
        for(r_index = 1; r_index <= MAX_CORES_PER_NODE; r_index = r_index << 2){
  //       3nd loop: r_index: 1 to MAX_CORES_PER_NODE by increments of 4x
           for(core_index = r_index; core_index <= MAX_CORES_PER_NODE; core_index = core_index << 2){
  //       4rd loop: core_index: r_index to MAX_CORES_PER_NODE by increments 4x
  //       Compute both speedup_asymmetric and speedup_dynamic and output results
           speedup_asymmetric = node_index * 1/((1 - f[f_index])/PERF(r_index) + f[f_index]/(PERF(r_index) + core_index - r_index)); 
           speedup_dynamic = node_index * 1/((1 - f[f_index])/PERF(r_index) + f[f_index]/core_index);
           printf("%10.8lf %8u %6u %6u %6u %12.4lf %12.4lf \n",
                  f[f_index], (node_index * core_index),
                  node_index, r_index, core_index,
                  speedup_asymmetric, speedup_dynamic);
           }
        }
    }

  return;
}



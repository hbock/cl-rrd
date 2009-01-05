#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <rrd.h>

void graph_test(void);

int main(int argc, char **argv)
{
  graph_test();
  return 0;
}

void graph_test()
{
  int xsize = 800, ysize = 800;
  double ymin, ymax;
  char **data = NULL;
  char *graph_argv[] = {
    "graph",
    "graph.png",
    "--start",
    "1231129446",
    "--end",
    "1231144146",
    "DEF:lin=rrd-test.rrd:linear:AVERAGE",
    "DEF:sin=rrd-test.rrd:sine:AVERAGE",
    "LINE2:lin#00FF00",
    "LINE2:sin#FF0000"
  };
  
  rrd_clear_error();
  rrd_graph(10, graph_argv, &data, &xsize, &ysize, NULL, &ymin, &ymax);
  if(rrd_test_error() != 0) {
    fprintf(stderr, "librrd error: %s\n", rrd_get_error());
  } else {
    free(data);
    printf("success!! size(%d, %d)\n", xsize, ysize);
  }
}

//-------------------------------------
// Author: Babak Naimi, naimi.b@gmail.com
// Date (first version): March 2018
// Date (last update):  March 2018
// Version 0.2
// Licence GPL v3
//------------------------



#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
IntegerVector cellNeighbours(int cell, int nr, int nc) {
  int row = floor((cell-1) / nc) + 1;
  int col = cell - ((row-1) * nc);
  
  IntegerVector r = IntegerVector::create(-1,-1,-1,0,1,1,1,0);
  IntegerVector c = IntegerVector::create(-1,0,1,1,1,0,-1,-1);
  
  r = row - r;
  c =  col - c;
  
  r[r < 1 | r > nr] = NA_INTEGER;
  c[c < 1 | c > nc] = NA_INTEGER;
  c = (r-1) * nc + c; 
  return c[!is_na(c)];
}

NumericVector c_n(NumericVector a, NumericVector b) {
  for (int i=0;i < b.size();i++) {
    a.push_back(b[i]);
  }
  return a;
}



IntegerVector c_int(IntegerVector a, IntegerVector b) {
  for (int i=0;i < b.size();i++) {
    a.push_back(b[i]);
  }
  return a;
}



IntegerVector which(LogicalVector x) {
  int nx = x.size();
  std::vector<int> y;
  y.reserve(nx);
  
  for(int i = 0; i < nx; i++) {
    if (x[i] == true) y.push_back(i+1);
  }
  return wrap(y);
}



    
// [[Rcpp::export]]
IntegerVector group(NumericVector x, int nr, int nc) {
  IntegerVector cells = Rcpp::seq(1,x.size());
  IntegerVector g(x.size());
  g.fill(NA_INTEGER);
  cells=cells[!is_na(x)];
  int groupID = 0, cell;
  IntegerVector NeighCells;
  IntegerVector cellN, w, sq;
  LogicalVector ww;
  NumericVector xx;
  
  while(cells.size() > 0) {
    groupID++;
    cell=cells[0];
    g[(cell - 1)]=groupID;
    sq=seq(0,cells.size()-1);
    cells=cells[sq != 0];
    cellN = cellNeighbours(cell,nr,nc);
    xx = x[(cellN - 1)];
    ww = is_na(xx);
    cellN = cellN[!ww];
    xx = xx[!ww];
    cellN = cellN[xx == x[(cell-1)]];
    if (cellN.size() > 0) NeighCells=c_int(NeighCells,cellN);
    
    while (NeighCells.size() > 0) {
      cell=NeighCells[0];
      sq=seq(0,NeighCells.size()-1);
      NeighCells=NeighCells[sq != 0];
      w=cells[cells == cell];
      if (w.size() > 0) {
        cells=cells[cells != cell];
        g[cell-1]=groupID;
        cellN = cellNeighbours(cell,nr,nc);
        xx = x[(cellN - 1)];
        ww = is_na(xx);
        cellN = cellN[!ww];
        xx = xx[!ww];
        cellN = cellN[xx == x[(cell-1)]];
        if (cellN.size() > 0) NeighCells=c_int(NeighCells,cellN);
        }
      }
      
  }
    return g;
}

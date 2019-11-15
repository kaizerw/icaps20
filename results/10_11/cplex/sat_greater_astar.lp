\ENCODING=ISO-8859-1
\Problem name: IloCplex

Minimize
 obj:  move_loc_x0_y0_x0_y1 + move_loc_x0_y0_x1_y0 + move_loc_x1_y0_x1_y1 + move_loc_x1_y0_x2_y0 + move_loc_x1_y0_x0_y0 + 
       move_loc_x2_y0_x2_y1 + move_loc_x2_y0_x1_y0 + move_loc_x0_y1_x0_y2 + move_loc_x0_y1_x0_y0 + move_loc_x0_y1_x1_y1 + 
       move_loc_x1_y1_x1_y2 + move_loc_x1_y1_x1_y0 + move_loc_x1_y1_x2_y1 + move_loc_x1_y1_x0_y1 + move_loc_x2_y1_x2_y2 + 
       move_loc_x2_y1_x2_y0 + move_loc_x2_y1_x1_y1 + move_loc_x0_y2_x0_y1 + move_loc_x0_y2_x1_y2 + move_loc_x1_y2_x1_y1 + 
       move_loc_x1_y2_x2_y2 + move_loc_x1_y2_x0_y2 + move_loc_x2_y2_x2_y1 + move_loc_x2_y2_x1_y2
Subject To
 c1:  move_loc_x1_y1_x1_y2 + move_loc_x1_y2_x2_y2 + move_loc_x2_y2_x2_y1 + move_loc_x2_y1_x2_y0 + 
      move_loc_x2_y0_x1_y0 + move_loc_x1_y0_x0_y0 + move_loc_x0_y0_x0_y1 + move_loc_x0_y1_x0_y2 >= 1
 c2:  move_loc_x1_y1_x1_y2 + move_loc_x1_y2_x0_y2 + move_loc_x0_y2_x0_y1 + move_loc_x0_y1_x0_y0 + 
      move_loc_x0_y0_x1_y0 + move_loc_x1_y0_x2_y0 + move_loc_x2_y0_x2_y1 + move_loc_x2_y1_x2_y2 >= 1
 c3:  move_loc_x1_y1_x1_y0 + move_loc_x1_y0_x0_y0 + move_loc_x0_y0_x0_y1 + move_loc_x0_y1_x0_y2 + 
      move_loc_x0_y2_x1_y2 + move_loc_x1_y2_x2_y2 + move_loc_x2_y2_x2_y1 + move_loc_x2_y1_x2_y0 >= 1
 c4:  move_loc_x1_y1_x1_y0 + move_loc_x1_y0_x2_y0 + move_loc_x2_y0_x2_y1 + move_loc_x2_y1_x2_y2 + 
      move_loc_x2_y2_x1_y2 + move_loc_x1_y2_x0_y2 + move_loc_x0_y2_x0_y1 + move_loc_x0_y1_x0_y0 >= 1
 c5:  move_loc_x1_y1_x2_y1 + move_loc_x2_y1_x2_y0 + move_loc_x2_y0_x1_y0 + move_loc_x1_y0_x0_y0 + 
      move_loc_x0_y0_x0_y1 + move_loc_x0_y1_x0_y2 + move_loc_x0_y2_x1_y2 + move_loc_x1_y2_x2_y2 >= 1
 c6:  move_loc_x1_y1_x2_y1 + move_loc_x2_y1_x2_y2 + move_loc_x2_y2_x1_y2 + move_loc_x1_y2_x0_y2 + 
      move_loc_x0_y2_x0_y1 + move_loc_x0_y1_x0_y0 + move_loc_x0_y0_x1_y0 + move_loc_x1_y0_x2_y0 >= 1
 c7:  move_loc_x1_y1_x0_y1 + move_loc_x0_y1_x0_y2 + move_loc_x0_y2_x1_y2 + move_loc_x1_y2_x2_y2 + 
      move_loc_x2_y2_x2_y1 + move_loc_x2_y1_x2_y0 + move_loc_x2_y0_x1_y0 + move_loc_x1_y0_x0_y0 >= 1
 c8:  move_loc_x1_y1_x0_y1 + move_loc_x0_y1_x0_y0 + move_loc_x0_y0_x1_y0 + move_loc_x1_y0_x2_y0 + 
      move_loc_x2_y0_x2_y1 + move_loc_x2_y1_x2_y2 + move_loc_x2_y2_x1_y2 + move_loc_x1_y2_x0_y2 >= 1
      
 c9:  move_loc_x0_y0_x1_y0 = 0
 c10: move_loc_x1_y0_x0_y0 = 0
 c11: move_loc_x2_y0_x2_y1 = 0
 c12: move_loc_x0_y1_x0_y2 = 0
 c13: move_loc_x1_y1_x0_y1 = 0
 c14: move_loc_x2_y1_x2_y0 = 0
 c15: move_loc_x0_y2_x1_y2 = 0
 c16: move_loc_x1_y2_x2_y2 = 0
Bounds
 0 <= move_loc_x0_y0_x0_y1 <= 1
 0 <= move_loc_x0_y0_x1_y0 <= 1
 0 <= move_loc_x1_y0_x1_y1 <= 1
 0 <= move_loc_x1_y0_x2_y0 <= 1
 0 <= move_loc_x1_y0_x0_y0 <= 1
 0 <= move_loc_x2_y0_x2_y1 <= 1
 0 <= move_loc_x2_y0_x1_y0 <= 1
 0 <= move_loc_x0_y1_x0_y2 <= 1
 0 <= move_loc_x0_y1_x0_y0 <= 1
 0 <= move_loc_x0_y1_x1_y1 <= 1
 0 <= move_loc_x1_y1_x1_y2 <= 1
 0 <= move_loc_x1_y1_x1_y0 <= 1
 0 <= move_loc_x1_y1_x2_y1 <= 1
 0 <= move_loc_x1_y1_x0_y1 <= 1
 0 <= move_loc_x2_y1_x2_y2 <= 1
 0 <= move_loc_x2_y1_x2_y0 <= 1
 0 <= move_loc_x2_y1_x1_y1 <= 1
 0 <= move_loc_x0_y2_x0_y1 <= 1
 0 <= move_loc_x0_y2_x1_y2 <= 1
 0 <= move_loc_x1_y2_x1_y1 <= 1
 0 <= move_loc_x1_y2_x2_y2 <= 1
 0 <= move_loc_x1_y2_x0_y2 <= 1
 0 <= move_loc_x2_y2_x2_y1 <= 1
 0 <= move_loc_x2_y2_x1_y2 <= 1
Binaries
 move_loc_x0_y0_x0_y1
 move_loc_x0_y0_x1_y0
 move_loc_x1_y0_x1_y1
 move_loc_x1_y0_x2_y0
 move_loc_x1_y0_x0_y0
 move_loc_x2_y0_x2_y1
 move_loc_x2_y0_x1_y0
 move_loc_x0_y1_x0_y2
 move_loc_x0_y1_x0_y0
 move_loc_x0_y1_x1_y1
 move_loc_x1_y1_x1_y2
 move_loc_x1_y1_x1_y0
 move_loc_x1_y1_x2_y1
 move_loc_x1_y1_x0_y1
 move_loc_x2_y1_x2_y2
 move_loc_x2_y1_x2_y0
 move_loc_x2_y1_x1_y1
 move_loc_x0_y2_x0_y1
 move_loc_x0_y2_x1_y2
 move_loc_x1_y2_x1_y1
 move_loc_x1_y2_x2_y2
 move_loc_x1_y2_x0_y2
 move_loc_x2_y2_x2_y1
 move_loc_x2_y2_x1_y2
Generals
End

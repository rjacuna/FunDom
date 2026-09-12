// HypTriangle.java  (part of FunDomain java program)

/* --------------------------------------------------------------

   FunDomain: Program for drawing fundamental domains of subgroups of SL_2(Z)
   Copyright (C) 2001  Helena A. Verrill

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU General Public License
   as published by the Free Software Foundation; either version 2
   of the License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

   Helena A. Verrill

   UK address:
   23 Roper Close
   Rugby
   CV21 4PF
   England
   email: verrill@math.ku.dk

   see web page: http://hverrill.net
   for most recent address and email address.   

   more information about the GPL can be found at:
   http://www.gnu.org/copyleft/gpl.html

------------------------------------------------------------------ 
*/


import java.awt.*;

public class HypTriangle{
    
    //object data

    
    public IntMat M;   // measures transform from identity triangle
    public int translation;
    public int length;
    public double[][] Triangle = new double [200][2];
    public ArcSection Arc1, Arc2, Arc3;
    int[]  X = new int [200];
    int[]  Y = new int [200];
    int Ma, Mc;
    double Scale;
    public double MaxY=0;
    public Polygon Poly = new Polygon();
    private double x0im, X0im, y0im, x1im, y1im, x2im, y2im;
    private double radius1, radius2, radius3;
    private double center1, center2, center3;
    private int top;
    static final double sqrt3 = 1.7320508075688772;
    private static Polygon[]         triangle_list = new Polygon [200];
    private static HypTriangle[] hyp_triangle_list = new HypTriangle [200];
    private static IntMat[]        hyp_matrix_list = new IntMat [200];
    private static int number_of_stored_triangles=0;


    //constructor
    
    public HypTriangle(int top, int width, int scale, IntMat N){
	this.top = top;
	this.Scale = 0;

	if (N.c==0) {
	    IntMat MMM = new IntMat(1,0,0,1);
	    this.M = MMM;  this.translation = N.b/N.d;}
	else {
	    this.translation = N.a/N.c;
	    IntMat MMM = new IntMat(1,-translation,0,1).mult(N);
	    M = MMM;
	}
	boolean found = false;
	int i=0;
	while ((found==false) && (i< number_of_stored_triangles)){
	    if (hyp_matrix_list[i].equal(M) || hyp_matrix_list[i].equal(M.negate())) found = true;
	    i++;
	}
	if (number_of_stored_triangles==0) i=0;
	if (found == true) {
	    this.Poly = triangle_list[i-1];
	    this.length = hyp_triangle_list[i-1].length;
	    this.Triangle = hyp_triangle_list[i-1].Triangle;
	    this.radius1 = hyp_triangle_list[i-1].radius1;
	    this.radius2 = hyp_triangle_list[i-1].radius2;
	    this.radius3 = hyp_triangle_list[i-1].radius3;
	    this.MaxY = hyp_triangle_list[i-1].MaxY;
	    this.Scale = hyp_triangle_list[i-1].Scale;
	    this.Ma = hyp_triangle_list[i-1].Ma;
	    this.Mc = hyp_triangle_list[i-1].Mc;
	}
	else{ 
	    find_double_triangle(scale);
	    find_Poly(scale, top, width);
	    if (i<199) {
		hyp_triangle_list[i] = this;
		triangle_list[i] = this.Poly;
		IntMat MMM = new IntMat(M.a,M.b,M.c,M.d);
		hyp_matrix_list[i] = MMM;
		number_of_stored_triangles++;
	    }
	}
    }
    
    private void find_double_triangle(int scale){

	IntMat N = new IntMat();
	double max = (double)(this.top)/(double)(scale);	

	if (M.c!=0)  {Ma = M.a; Mc=M.c;}
	else {Ma = M.b; Mc=M.d;}
	
	N = new IntMat(Mc,-Ma,0,Mc).mult(M);

	// image of infinity
	
	x0im = N.XIm("infinity",-1); 
	X0im = N.XIm("infinity",1); 
	y0im = N.YIm("infinity",max); 
	
	// image of first elliptic point
	
	x1im = N.XIm(-0.5,sqrt3/2);
	y1im = N.YIm(-0.5,sqrt3/2);
	
	// image of second elliptic point
	
	x2im = N.XIm(0.5,sqrt3/2);
	y2im = N.YIm(0.5,sqrt3/2);
	
	if (M.c==0) {x0im = x1im;
	X0im = x2im;}
	
	radius1 = Math.abs((x0im - N.XIm(-0.5,0))/2);
	radius2 = Math.abs((N.XIm(1,0) - N.XIm(-1,0))/2);
	radius3 = Math.abs((X0im - N.XIm(0.5,0))/2);
	center1 = (x0im + N.XIm(-0.5,0))/2;
	center2 = ((double)(N.a*N.c-N.b*N.d))/((double)(N.c*N.c-N.d*N.d));
	center3 = (X0im + N.XIm(0.5,0))/2;

	Arc1 = new ArcSection(x0im,y0im,x1im,y1im,radius1,center1);

	IntMat NS = new IntMat();
	NS = N.mult(IntMat.S);

	boolean nbr = false;
	int i=0;
	while ((i<number_of_stored_triangles) && (!nbr)){
	    if ((NS.div(hyp_matrix_list[i])).c==0) nbr = true;
	    i++;
	}
	if (nbr == true){
	    Arc2 = new 
		ArcSection(hyp_triangle_list[i-1].Arc2,
			   -x1im+hyp_triangle_list[i-1].x2im);}
	else{
	    Arc2 = new ArcSection(x1im,y1im,x2im,y2im,radius2,center2);}
	Arc3 = new ArcSection(x2im,y2im,x0im,y0im,radius3,center3);
	
	length = Arc1.length + Arc2.length + Arc3.length;

	MaxY = Math.max(Math.max(Arc1.MaxY,Arc2.MaxY),Arc3.MaxY);
	Triangle = ArcSection.concatinate(Arc1,Arc2,Arc3);
    
	
    }
	    

    public void find_Poly (int scale, int top, int width){

	if (Scale == (double)scale) return;
	Scale = (double) scale;
	


	int i,j,s;
	
	// next line needs to be changed!
	// (the point is, if the size of the drawing area is changed,
	// this should change)

	this.top = top;

	if (Mc==0){
	    if (scale > 1000){
		Polygon P = new Polygon(X,Y,0);
		Poly =  P;
	    }
	}
	else {
	    double MaxRadius = Math.max(Math.max(radius1,radius2),radius3);
	    
	    double max = (double)top/Scale;
	    double Size  = MaxRadius*Scale; 
	    double lft_lim = -(double)2*width/Scale-2*MaxRadius;
	    double rt_lim =   (double)2*width/Scale+2*MaxRadius;
	    // the left and rt limits here for X values are assumnig we've
	    // moved so the image of infinity is on the screen.
	    // these values could be made more accurate.
	    // have cut this out until done properly

	    for(i=0;i<=length;i++){
         X[i] =(int)Math.round((Triangle[i][0]*Scale));
         Y[i] =top - (int)Math.round((Math.max(Math.min(Triangle[i][1],max),-max)*Scale));
	    }
	    
	    Polygon P = new Polygon(X,Y,length);
	    
	    Poly = P;
	    Poly.translate((Ma*scale)/Mc,0);
	}
    }
	    


    public boolean appears_on_Screen(int scale, int x0, int width){
	
	return true;
	
    }


}










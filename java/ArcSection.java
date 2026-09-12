// ArcSection.java (part of FunDomain java program)

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

public class ArcSection{
    
    //object data

    public double x0, y0, x1, y1;
    public double center, radius;
    public double[][] XYcords = new double [101][2];
    public double MaxY;
    public int length;
    
    //static data - the identity triangle

    private static double  circleX[] = new double [101]; 
    private static double  circleY[] = new double [101];
				  
    private double Xdubs[] = new double [101];
    private double Ydubs[] = new double [101];

    // we fix some points on a circles

    static {
	int i,j,lim,e;
    	int N=3;
	int Max_e=16;
	int first_div = 24;
	// (first think about dividing circle into 24 sections)
	int biggest_gap = 0;
	// (this means biggest_gap is 2^(0)/first_div)
	double ik;
	int s = 0;
	double angle = 0;
	double fraction = 0;

	double div = Math.pow(2,Max_e)*first_div;
	for (e = Max_e;e>=biggest_gap;e--){
	    if (e==Max_e) lim=6;
	    else if (e==biggest_gap) lim=(int)(div-3+1); 
	    else lim = 3;
	    for (j=1;j<=lim;j++){
		circleX[s]=  Math.cos(angle);
		circleY[s]=  Math.sin(angle);
		fraction =  fraction + ((double)1)/div;
		angle = -fraction*Math.PI + Math.PI;
		s++;
	    }
	    div = div/2;
	}

    }
    
    
    //constructor
    
    public ArcSection(ArcSection A1,  double translate){
    
	// note, this also changes direction of going along
	// arc, as this is needed in application in
	// HypTriangle class
	
	x0=A1.x0 - translate;
	y0=A1.y0;
	x1=A1.x1 - translate;
	y1=A1.y1;
	center = A1.center - translate;
	radius = A1.radius;
	MaxY= A1.MaxY;
	length = A1.length;
	int i;
	for (i=0;i<length;i++){
        XYcords[i][0] =  A1.XYcords[length-i-1][0] - translate;
        XYcords[i][1] =  A1.XYcords[length-i-1][1];
	}
    }
     
    

	

    public ArcSection(double x0,  double y0,  
		      double x1,  double y1,
		      double radius,
		      double center) {

	this.x0 = x0;
	this.y0 = y0;
	this.x1 = x1;
	this.y1 = y1;
	this.radius = radius;
	if (radius < 0) this.radius = - radius;
	this.center = center;
	this.MaxY = this.findMaxY();
	this.findXYcords();
    }


    private  double findMaxY(){
	double start_x, end_x;
	double start_y, end_y;

	start_x = Math.min(x0,x1);
	end_x = Math.max(x0,x1);

	if (x0 == x1) return Math.max(y0,y1);
	
	else if ((x0 < center) && (x1 > center)) return radius;

	else return Math.max(y0,y1);
	
    }

    
    private void find_circle (){

	int i;
	
	for (i=0;i<=72;i++){
	    Xdubs[i] = circleX[i]*radius + center;
	    Ydubs[i] = circleY[i]*radius;
	}   
    }

    private void findXYcords(){

	if ((radius==0) || (x0==x1)){
	    XYcords[0][0]= x0;
	    XYcords[0][1]= y0;
	    XYcords[1][0]= x0;
	    XYcords[1][1]= y1;
	    length = 2;
	}

	else{
	    int swapped[] = {0,0,0};
	    
	    double Xswap, Yswap;
	    double X0 = x0;
	    double X1 = x1;
	    double Y0 = y0;
	    double Y1 = y1;
	    int i,s;
	    

	    if ((center < 0) && (X0 < X1)) {
		center = -center; 
		Xswap = X0; X0 = -X1; X1 = -Xswap;
		Yswap = Y1; Y1= Y0; Y0 = Yswap; swapped[0]=1;}
	    
	    else if ((center < 0) && (X0 > X1)) {
		X1 = -X1; X0 = -X0;
		center = -center; swapped[1]=1;}
	    
	    else if ((center >= 0) && (X0 > X1)) {
		Xswap = X1; X1= X0; X0 = Xswap;
		Yswap = Y1; Y1= Y0; Y0 = Yswap; swapped[2]=1;}
	    
	    find_circle();
	    
	    s=1;
	    
	    // this assumes we have switched so X2 > X1

	    for(i=0;i<=72;i++){
	    	if (Xdubs[i]>=X0 && Xdubs[i]<=X1){
		    XYcords[s][0] = Xdubs[i];
		    XYcords[s][1] = Ydubs[i];
		    s++;
		}
	    }
	
	    XYcords[0][0] = X0;  
	    XYcords[0][1] = Y0;  
	    XYcords[s][0] = X1;  
	    XYcords[s][1] = Y1;

	    length = s+1;
	    
	    if ((swapped[0]==1) || (swapped[1]==1)) {
		for (i=0;i<=s;i++) {
		    XYcords[i][0] = -XYcords[i][0];
		}}
	    
	
	    if ((swapped[0]==1) || (swapped[2]==1) ) {
		for (i=0;i<=s/2;i++) {
		    Xswap = XYcords[i][0];
		    XYcords[i][0] = XYcords[s-i][0];
		    XYcords[s-i][0] = Xswap;
		    Yswap = XYcords[i][1];
		    XYcords[i][1] = XYcords[s-i][1];
		    XYcords[s-i][1] = Yswap;
		}
	    }
	}
    }


    public static double[][] concatinate (ArcSection Arc1,
					  ArcSection Arc2,
					  ArcSection Arc3){
	int i;
	int length1 = Arc1.length;
	int length2 = Arc2.length;
	int length3 = Arc3.length;

	int length = length1 + length2 + length3;

	//double XYlist[][] = new double [length][2];
	double XYlist[][] = new double [200][2];

	for (i = 0; i<length1; i++){
	    XYlist[i][0] = Arc1.XYcords[i][0];
	    XYlist[i][1] = Arc1.XYcords[i][1];
	}

		for (i = 0; i<length2; i++){
	   XYlist[i+length1][0] = Arc2.XYcords[i][0];
	   XYlist[i+length1][1] = Arc2.XYcords[i][1];
	}

	for (i = 0; i<length3; i++){
	    XYlist[i+length1+length2][0] = Arc3.XYcords[i][0];
	    XYlist[i+length1+length2][1] = Arc3.XYcords[i][1];
	}
	
	return XYlist;
    }




    public boolean Intersects_Screen(int scale, int zero, 
				     int height, int width){
	
	return true;
	

    }


}



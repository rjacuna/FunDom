// IntMat.java  (part of FunDomain java program)

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

// This class defines integer 2 by 2 matrices, and various operations on them


// For future: might be better to define an extension class of 
// fractional linear transformations (ie, different definition of equality)

public class IntMat{

    public int a,b,c,d;

    //constructor

    public IntMat(){
    this.a=1;
    this.b=0;
    this.c=0;
    this.d=1;
    }

    public IntMat(int a, int b, int c, int d){
	this.a=a;
	this.b=b;
	this.c=c;
	this.d=d;
    }

    // some 'constant' matrices
    
    static public final IntMat T = new IntMat(1,1,0,1);

    static public final IntMat Tm = new IntMat(1,-1,0,1);

    static public final IntMat S = new IntMat(0,-1,1,0);

    static public final IntMat R = new IntMat(0,-1,1,1);

    static public final IntMat Id = new IntMat(1,0,0,1);
	
    
    //methods: multiply

    public IntMat mult(IntMat B){
	int a1,b1,c1,d1;

	a1 = a * B.a + b * B.c;
	b1 = a * B.b + b * B.d;
	c1 = c * B.a + d * B.c;
	d1 = c * B.b + d * B.d;

        IntMat M = new IntMat(a1,b1,c1,d1);
	return M;
    }

    public static IntMat T(int n){
	IntMat N = new IntMat(1,n,0,1);
	return N;
    }


    public static IntMat mult(IntMat A, IntMat B){
	int a1,b1,c1,d1;

	a1 = A.a * B.a + A.b * B.c;
	b1 = A.a * B.b + A.b * B.d;
	c1 = A.c * B.a + A.d * B.c;
	d1 = A.c * B.b + A.d * B.d;

        IntMat M = new IntMat(a1,b1,c1,d1);
	return M;
    }

    //methods: determinant

    public int det(){
	return a*d - b*c;
    }


    //methods: invert

    public IntMat inv(){
	//??? need to learn about 'exceptions' perhaps. if (det()!=1) ???;
	
        IntMat M = new IntMat(d,-b,-c,a);
	return M;
    }

    //methods: divide

    public IntMat div(IntMat B){
	//??? how to cope with exceptions like det B = 0??

	int a1,b1,c1,d1;

	a1 =  a * B.d - b * B.c;
	b1 = -a * B.b + b * B.a;
	c1 =  c * B.d - d * B.c;
	d1 = -c * B.b + d * B.a;

        IntMat M = new IntMat(a1,b1,c1,d1);
	return M;
    }

    //??? There seem to be some errors coming up associated to the
    // following function.  What's wrong???

    public static IntMat div(IntMat A, IntMat B){
	//??? how to cope with exceptions like det B = 0??

	int a1,b1,c1,d1;

	a1 =  A.a * B.d - A.b * B.c;
	b1 = -A.a * B.b + A.b * B.a;
	c1 =  A.c * B.d - A.d * B.c;
	d1 = -A.c * B.b + A.d * B.a;

        IntMat M = new IntMat(a1,b1,c1,d1);
	return M;
    }

    // method: check equality

    public boolean equal(IntMat A){
	if (this.a == A.a && 
	    this.b == A.b && 
	    this.c == A.c && 
	    this.d == A.d) return true;
	else return false;
    }

    // method: check equality as fractional linear transformation

    public boolean fracEqual(IntMat A){
	if ((this.a == A.a && 
	     this.b == A.b && 
	     this.c == A.c && 
	     this.d == A.d)||
	    (this.a == -A.a && 
	     this.b == -A.b && 
	     this.c == -A.c && 
	     this.d == -A.d))
	    return true;
	else return false;
    }

    // method: negate

    public IntMat negate(){
	IntMat M = new IntMat(-a,-b,-c,-d);
	return M;
    }


    // method: conjugate
    // note, it's assumed that after conjugating we still end up 
    // with an integer matrix.  This might cause problems if the
    // program is extended
    // also it's assumed det(B) not = 0
    // this will cause errors if this is not true.

    public void conjugate(IntMat B){
	int det = B.det(); 
	int newa = (-this.b*B.c + this.a*B.d)*B.a + (-this.d*B.c + this.c*B.d)*B.b; 
	int newb = this.b*B.a*B.a + (-this.a + this.d)*B.b*B.a - this.c*B.b*B.b;

	int newc = -this.b*B.c*B.c + (this.a - this.d)*B.d*B.c + this.c*B.d*B.d ;
	int newd = (this.b*B.c + this.d*B.d)*B.a + (-this.a*B.c - this.c*B.d)*B.b;

	this.a = newa/det;
	this.b = newb/det;
	this.c = newc/det;
	this.d = newd/det;

    }

    // image of x + iy from fractional transformation defined by M

    public double XIm(double x, double y){

	double A = (double) this.a;
	double B = (double) this.b;
	double C = (double) this.c;
	double D = (double) this.d;
	double denom;

	denom= (C*x+D)*(C*x+D)+C*C*y*y;
	if (denom!=0) return ((A*x+B)*(C*x+D) + A*C*y*y)/denom;
	else return (-A*x+B)*(-C*x+D)/((-C*x+D)*(-C*x+D));
	// this value of x if y is infinity is choosen so that
	// maxY can be defined properly in HypTriangle class
    }


    public double YIm(double x, double y){

	double A = (double) this.a;
	double B = (double) this.b;
	double C = (double) this.c;
	double D = (double) this.d;
	double denom;

	denom= (C*x+D)*(C*x+D)+C*C*y*y;
	if (denom!=0) return (-C*y*(A*x+B)+A*y*(C*x+D))/denom;
	else return YIm(0.5,HypTriangle.sqrt3/2);
	// the above choice is so that HypTri draws the triangle correctly
    }

    public double XIm(String infinity, int which){
	double A = (double) this.a;

	if (this.c!=0) {	
	    double C = (double) this.c;
	    return A/C;
	}
	else {
	    double B = (double) this.b;
	    double D = (double) this.d;
	    return (2*B+which*A)/D/2;
	}
    }

    public double YIm(String infinity, double max){

	if (this.c!=0) return 0;

	else return max;

    }

    public IntMat move_to_origin(){
	
	if (c==0) {
	    return IntMat.Id;
	}
	else {
	    int translation = a/c;
	    IntMat MMM = new IntMat(1,-translation,0,1).mult(this);
	    return MMM;
	}
    }

}






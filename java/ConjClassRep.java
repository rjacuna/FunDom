// ConjClassRep.java (part of FunDomain java program)

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
import java.awt.event.*;
import java.applet.*;

/*
 *  This is to define Coset represenatives of the  
 *  conjugacy classes; right now, just for Gamma_0(N)
 *  these are given as 2 by 2 matrices.
 *  "group" type doesn't actualy make any difference
 */

public class ConjClassRep extends IntMat{
    public Polygon triangle;
    public int translation;
    public double[] Imx = new double[3];
    public double[] Imy = new double[3];
    public static double sqrt2 = 1.4142135623730950488016887;

    // variable is a 2 * 2 integer matrix

    //constructor

    public ConjClassRep(int a, int b, int c, int d){
	super(a,b,c,d);
	setImx();
	setImy();
    }


    public ConjClassRep(){
	super();
    }

    //??? is this really the right way to define a kind of casting to
    //??? convert intmat to  conjclassrep ?

    public ConjClassRep(IntMat M){
	super(M.a,M.b,M.c,M.d);
	setImx();
	setImy();
    }

    //methods

    //when are two Reps equiv?

    // we'll assume that things are either in the form 
    // Gamma_0(N)
    // or Gamma_0(N) intersect Gamma_1(M), with N a multiple of
    // M.  This should be sorted out earlier so things are in this
    // form, and so we can conjugate to get like this.

    public boolean equiv(String grouptype1, String grouptype2, int N, int M, ConjClassRep B){
	
	return equiv(grouptype1,grouptype2,N,M,this,B);
    }
	
    public static boolean equiv(String grouptype1, String grouptype2, int N, int M, ConjClassRep A, ConjClassRep B){
    	IntMat Mat;
	int Ma,Mb,Mc;

	Mat = A.div(B);
	Mb=Mat.b; 
	Mc=Mat.c;
	Ma=Mat.a;

	if (N!=1){
	    if (grouptype1=="Gamma(N)" || grouptype1=="Gamma^0(N)" || grouptype1=="Gamma^1(N)"){
		if ((Mb/N)*N!=Mb) return false;
	    }
	    if (grouptype1=="Gamma(N)" || grouptype1=="Gamma^1(N)" || grouptype1=="Gamma_1(N)"){
		int A1 = Ma-1;
		int A2 = Ma+1;
		if ((A1/N)*N!=A1 && (A2/N)*N!=A2) return false;
	    }
	    if (grouptype1=="Gamma(N)" || grouptype1=="Gamma_1(N)" || grouptype1=="Gamma_0(N)"){
		if ((Mc/N)*N!=Mc) return false;
	    }
	}
	if (M!=1){
	    if (grouptype2=="Gamma(M)" || grouptype2=="Gamma^0(M)" || grouptype2=="Gamma^1(M)"){
		if ((Mb/M)*M!=Mb) return false;
	    }
	    if (grouptype2=="Gamma(M)" || grouptype2=="Gamma^1(M)" || grouptype2=="Gamma_1(M)"){
		int A1 = Ma-1;
		int A2 = Ma+1;
		if ((A1/M)*M!=A1 && (A2/M)*M!=A2) return false;
	    }
	    if (grouptype2=="Gamma(M)" || grouptype2=="Gamma_1(M)" || grouptype2=="Gamma_0(M)"){
		if ((Mc/M)*M!=Mc) return false;
	    }
	}
	return true;

    }

    // this is to say if something is equiv to a multiple of T.

    public  boolean equivT(String grouptype, int N){
	// this function is currently not used anywhere!
	
	int i;
	ModN modN = new ModN(N);
	int A = modN.mod(this.a);
	int B = modN.mod(this.b);
	boolean soluble = false;  //(can we solve a = tb mod N?)
	if (modN.invert[A]!=-1) soluble = true;
	else if (modN.invert[B]==-1){
	    for (i=1;i<N;i++){
		if (modN.mod(i*B)==A) soluble = true;
	    }
	}
	

	if ((grouptype=="Gamma^0(N)") ||
	    (grouptype=="Gamma^1(N)") ||
	    (grouptype=="Gamma(N)")){
	    if (soluble){
		if (grouptype=="Gamma^0(N)") return true;
		else if (((A+1)/N)*N==A+1 || ((A-1)/N)*N==A-1){
		    if (grouptype=="Gamma^1(N)") return true;
		    else if ((this.c/N)*N==this.c) return true; }
		return false;
	    }
	}
	else{
	    if ((this.c/N)*N==this.c){
		if (grouptype=="Gamma_0(N)") return true;
		else if (((A+1)/N)*N==A+1 || ((A-1)/N)*N==A-1){
		    if (grouptype=="Gamma^1(N)") return true;}
		return false;
	    }
	}
	return false;

    }
    
    // when do two reps have adjacent domains?

    public boolean adj(ConjClassRep B){
	IntMat Mat;
	Mat = IntMat.mult(IntMat.T,this);
	if (this.fracEqual(Mat)) return true;
	else {
	    Mat = IntMat.mult(IntMat.Tm,this);
	    if (this.fracEqual(Mat)) return true;
	    else {
		Mat = IntMat.mult(IntMat.S,this);
		if (this.fracEqual(Mat)) return true;
		else return false;
	    }
	}
    }

    // Make a list of adjacent domains

    public ConjClassRep[] adjList(){
	ConjClassRep adjreps[] = new ConjClassRep[3];
	adjreps[1] = (ConjClassRep) this.mult(IntMat.T,this);
	adjreps[2] = (ConjClassRep) this.mult(IntMat.Tm,this);
	adjreps[3] = (ConjClassRep) this.mult(IntMat.S,this);
	return adjreps;
    }

    
    // give the corresponding hyperbolic triangle

        public HypTriangle tri(int top, int width, int scale){ 
    	HypTriangle HT = new HypTriangle(top,width,scale,this); 
	triangle = HT.Poly;
	translation = HT.translation;
    	return HT;
    }

    
    // the following are images of points around edges of triangle
    // this is up to the translation of the cusp back to 0, or 0 + i*infinity

    public void setImx(){
	IntMat N = this.move_to_origin();
	Imx[0] = N.XIm(0.5,(sqrt2+1)*0.5);	
	Imx[1] = N.XIm(-0.5,(sqrt2+1)*0.5);	
	Imx[2] = N.XIm(0,1);	

    }

    public void setImy(){
	IntMat N = this.move_to_origin();
	Imy[0] = N.YIm(0.5,(sqrt2+1)*0.5);	
	Imy[1] = N.YIm(-0.5,(sqrt2+1)*0.5);	
	Imy[2] = N.YIm(0,1);	

    }


}












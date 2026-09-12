// ConFrac.java (part of FunDomain java program)

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
import java.util.*;
import java.awt.event.*;
import java.applet.*;

/*
 * This is to create a continuted fraction given a fraction
 * 
 */
public class ConFrac {

    public int a,b;
    public String expString = new String();
    public int length;
    public int[] expansion;  // this is the confrac expansion
    public int[] qpart;      // this is the coeffs of the other part
    //                       // of the matrices involved.
    public int gcf;
    // this is greatest common factor

    IntMat newmat, runningmat, newrun;

    public ConFrac(int a, int b) {
	this.a =  a;
	this.b =  b;
	findExpansion();	
    }

    private void findExpansion() {
		
	int u=a;
	int v=b;
	if (u<0) u = -u;
	if (u<0) v = -v;

	int v1;

	runningmat = new IntMat();

	expString = ""+(u/v);
	if (u>=v) {u = u-(u/v)*v;}

	length = 1;
	while(u>0)
	    { 
		length++;
		expString = expString+","+(v/u);
		newmat = new IntMat(1,0,0,-(v/u));
		
		runningmat.mult(newmat);

		v1=u;
		u=v-(v/u)*u;
		v=v1;
	    }
	gcf = v;
	expansion = new int[length];
	
	StringTokenizer tok = new StringTokenizer(expString,",");
	for (u=0;u<length;u++){
	    expansion[u] = Integer.parseInt(tok.nextToken());
	}
	
    }

}
	











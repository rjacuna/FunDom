// FunDomain.java

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




///////////////////////////////////////////////////////////////////////////
//
// Hyperbolic Triangle and fundamental domain drawer
//  this is the main class of the set of classes comprising this program.
//
//  The main part of the mathematical computations is contained in 
//  the "RepList.java"
//  The main part of the actual drawing of the picture is in the HypTriangle
//  class and in the ArcSection class.
//  
/////////////////////////////////////////////////////////////////////////


import java.awt.*;
import java.util.*;
import java.awt.event.*;
import java.applet.*;

public class FunDomain extends Applet {
    DomainControls controls;   
    UpperHalfPlane upperplane;    
    ColourControls colcontrol;
    private Button clear_button;


    public void init() {
	setLayout(new BorderLayout());
	upperplane = new  UpperHalfPlane();
	add("Center", upperplane);
	controls = new DomainControls(upperplane);
	controls.setSize(800,200);
	add("South", controls);
	add("East", colcontrol = new ColourControls(upperplane));
	
	//	clear_button = new Button("clear");

    }

    public void destroy() {
        remove(controls);
        remove(upperplane);
    }

    public void start() {
	controls.setEnabled(true);
	colcontrol.setEnabled(true);
    }

    public void stop() {
	controls.setEnabled(false);
    }

    public void processEvent(AWTEvent e) {
        if (e.getID() == Event.WINDOW_DESTROY) {
            System.exit(0);
        }
    }

    public static void main(String args[]) {
	Frame f = new Frame("FunDomain");
	FunDomain  domains = new FunDomain();

	domains.init();
	domains.start();

	f.add("Center", domains);
	f.setSize(800, 500);
	f.show();
    }

}

class littlelable extends Canvas{
    private String text1, text2;
    private Font font;
    private int ht,width;
    private int pos1,pos2;

    public littlelable(String text1, String text2, int ht,int width,
		       int pos1, int pos2) {
	this.text1 = text1;
	this.text2 = text2;
	this.ht = ht;
	this.width = width;
	this.pos1=pos1;
	this.pos2=pos2;

	//	setBackground(Color.white);
    }

    public void paint(Graphics lab_g){
	font = new Font("serif", Font.BOLD, 14 );
	setSize(width,ht);
       
	lab_g.setColor(Color.blue);

	int lab_h = getBounds().height;
	int lab_w = getBounds().width;
	lab_g.fillOval(15,ht-40,lab_w-15,lab_h-(ht-40));
	lab_g.setColor(Color.yellow);
	lab_g.setFont(font);
	lab_g.drawString(text1,pos1,(ht-22)); //53
			 lab_g.drawString(text2,pos2,ht-7);

    }
    

}

class UpperHalfPlane extends Canvas implements MouseListener, MouseMotionListener{
    private Font	font;
    int         scale=50;
    int         oldscale=0;
    int         N=1;
    int         M=1;
    int      oldN=-1;
    int      oldM=-1;
    int last_x, last_y;
    int end_x, end_y;
    private int Xcords[] = {0,0,0,0};
    private int Ycords[] = {0,0,0,0};
    boolean is_a_rectangle;  // whether or not the user has drawn 
                             // a rectangle on the screen 
    boolean editmode=false;  // in editmode, there will be circles added
                             // where triangles have 'open' edges.
    boolean linkmode=false;
    Rectangle r = getBounds();    
    Polygon Polylist[] = new Polygon[2000];
    int translist[] = new int[2000];
    // int Width = r.width;
    // int Height = r.height;
    // Can't do this here, since they'll just be zero - not useful

    // need to at some stage change program to update when Heigth and
    // Width change.
    

    int Width=850;
    int Height=450;


    
    int         x0=600/2;  
    int         y0=100;

    private HypTriangle HT; //= new HypTriangle(Height,Width,scale,IntMat.Id);
    private String grouptype = "Gamma_0(N)";  // this is the starting choice
    //                                        // of group type
    private String grouptype2 = "Gamma_0(M)";  // this is the starting choice
    //                                        // of group type
    private String oldgrouptype = "no group";
    private String oldgrouptype2 = "no group";

    RepList RRR = new RepList(grouptype,grouptype2);
    int linklist[][][] = new int [500][3][3];
    //(link list gives the (x3,y3) coords of the link passing through
    // (x1,y1), (x2,y2).  The user can change the position of (x3,y3)
    // the [500] is for 500 possible triangles
    // the first [3] is for the sides T, Tm, S
    // the last [3] is for (1) link on/off (2) x3 (3) y3.
    int Replength=1;
    Color colour1=Color.red;
    Color colour2=Color.black;
    boolean screenclear=false;
    boolean totalclear=false;
    boolean newtriangles=false;
    boolean trianglemode=false;

    public UpperHalfPlane() {
	setBackground(Color.white);
	addMouseMotionListener(this);
	addMouseListener(this);
    }

    public void update(Graphics g) {
	Rectangle r = getBounds();    
	Width = r.width;
	Height = r.height;
	int i,H;
	int l = RRR.length();
	Replength = l;
	int   xp2[]={0,0,r.width,r.width};
	int   yp2[]={0,r.height,r.height,0};

	is_a_rectangle=false;
	if(screenclear==true){
	    g.setColor(Color.white);
	    Polygon poly0 = new Polygon(xp2,yp2, 4);       
	    g.fillPolygon(poly0);        

	    H=r.height;

	    g.setColor(Color.black);
	    g.drawLine(0,H - y0,r.width,H - y0);
	    g.drawLine(x0,H - y0,x0,H - y0+5);
	    if (scale < 2000){
		g.drawLine(x0+scale,H - y0,x0+scale,H - y0+5);}
	    
	    g.setFont(font);
	    g.drawString("0", x0-2, H-y0+15);
	    if (scale < 2000){
		g.drawString("1", x0-2+scale, H-y0+15);}


	    int spacing=2;
	    if (50<x0) spacing = 10;
	    g.drawLine(50,H - y0,50,H - y0+5);
	    float xx = ((float)(50-x0))/((float) scale);
	    g.drawString(""+xx, 50-spacing, H-y0+15);

	    g.drawLine(550,H - y0,550,H - y0+5);
	    if (550<x0) spacing = 10; else spacing =2;
	    xx = ((float)(550-x0))/((float) scale);
	    g.drawString(""+xx, 550-spacing, H-y0+15);



	    if (trianglemode == false){
		String grouptypetext;
		if (grouptype=="Gamma(N)") grouptypetext = "(";
		else grouptypetext = grouptype.substring(5,8);

		String grouptypetext2;
		if (grouptype2=="Gamma(M)") grouptypetext2 = "(";
		else grouptypetext2 = grouptype2.substring(5,8);

		// note: following should be improved, eg, in case
		// where the two groups can be intersected to give
		// a single grouptype, not written as an intersection
		if (M==1) {g.drawString("Group: Gamma" + grouptypetext + N +")", 20, H-55);}
		else if (N==1) {g.drawString("Group: Gamma" + grouptypetext2 + M +")", 20, H-55);}
		else {g.drawString("Group: Gamma" + grouptypetext + N +") intersect Gamma" + grouptypetext2 + M +")", 20, H-55);}

	    g.drawString("Genus: " + RRR.genus, 20, H-40);
	    g.drawString("Cusps: " + RRR.CuspNo+": ", 20, H-25);
	    g.drawString("Index: " + l + " (projective index)", 20, H-10);

	    font = new Font("sanSerif", Font.PLAIN, 8 );	    
	    g.setFont(font);
	    int len = 20+6*("Cusps: " + RRR.CuspNo+": ").length();
	    int xl = 0;
	    int yl=0;
	    for (i=0;i<RRR.CuspNo;i++){
		g.drawString(" "+RRR.cusps[i][0],len,H-30);
		xl = 0;
		if (RRR.cusps[i][0]<0) xl = 4;
		yl = Math.max((" "+RRR.cusps[i][0]).length(),
			   (" "+RRR.cusps[i][1]).length());
		if (RRR.cusps[i][1]!=1){
		    g.drawString(" "+RRR.cusps[i][1],len+xl,H-21);
		    g.drawLine(len+1,H-29,len+5*yl-2,H-29);}
		len = len + 5*yl;
	    }
	    font = new Font("sanSerif", Font.PLAIN, 10 );	    
	    g.setFont(font);
	    g.drawString("widths: ", len+5, H-25);
	    len = len + 6*("widths: ").length()+5;
	    for (i=0;i<RRR.CuspNo-1;i++){
		g.drawString(""+RRR.cusps[i][2]+", ",len,H-25);
		len = len + 6*(""+RRR.cusps[i][2]+", ").length();
	    }
	    g.drawString(""+RRR.cusps[RRR.CuspNo-1][2],len,H-25);
	    }



	    if (trianglemode == true){
		IntMat Mat = new IntMat();
		Mat = (IntMat) RRR.reps[l-1];
		int sx = 580; 
		int sy = H-22;
		drawMatrix(g, Mat,sx, sy);
	    }
	}
    

	if (totalclear==false){

	    for(i=0;i<l;i++){
		if ((scale!=oldscale)||(N!=oldN)||(M!=oldM)||(grouptype!=oldgrouptype)||(grouptype2!=oldgrouptype2)){
		    HT = RRR.reps[i].tri(r.height,Width,scale);
		    
		    //	    if(HT.appears_on_Screen(scale,x0,r.width)){
		    
		    HT.find_Poly(scale,Height,r.width);
		    Polylist[i]=HT.Poly;
		    translist[i]=HT.translation;

		}

		g.translate(x0,-y0);
		g.translate(translist[i]*scale,0);
		if (trianglemode==false) g.setColor(colour1);
		else g.setColor(colour1);
		g.fillPolygon(Polylist[i]);
		g.setColor(colour2);
		//  if ((colour1==Color.black)||(colour2==Color.black))
		//    g.setColor(Color.yellow);
		g.drawPolygon(Polylist[i]);
		g.translate(-translist[i]*scale,0);
		g.translate(-x0,y0);
	    }
	    
	    if (linkmode && !trianglemode) drawlinks("draw");
	    if (editmode && !trianglemode) drawdots(Color.yellow);
	    else {
		g.setColor(Color.white);
		g.fillRect(20, Height-80,90,10);}
	}

	screenclear = false;
	totalclear = false;

    }
    
    
    private void drawMatrix(Graphics g,IntMat Mat, int sx, int sy){

	    String m11,m12,m22,m21;
	    m11 = " " +  Mat.a;
	    m21 = " " +  Mat.c;
	    m12 = " " + Mat.b;
	    m22 = " " + Mat.d;
	    int space = 
		(Math.max(m11.length(),m21.length())+1)*6;
	    g.drawString("M = ", sx-28, sy-18);
	    g.drawString(m11, sx-3, sy-18);
	    g.drawString(m21, sx-3, sy-3);
	    g.drawString(m12, sx-3+space, sy-18);
	    g.drawString(m22, sx-3+space, sy-3);
	    int   bracketlx[]={sx,sx-3,sx-3,sx};
	    int   bracketly[]={sy,sy,sy-27,sy-27};
	    g.drawPolyline(bracketlx,bracketly,4);
	    int   bracketrx[]={sx+3,sx,sx+3,sx+3,sx,sx+3};
	    int   bracketry[]={sy,sy,sy,sy-27,sy-27,sy-27};
	    Polygon bracketR = new Polygon(bracketrx,bracketry,6);
	    space = space +
		Math.max(m12.length(),m22.length())*6;
	    bracketR.translate(space,0);
	    g.drawPolygon(bracketR);
    }



    public void drawlinks(String draw){
	int i,j,Mj;
	int l = RRR.length();
	Graphics g = getGraphics();

	for (i=0;i<l;i++){
	    // T neighbour:	    
	    if(RRR.table[i][1][2]!=1){		
		if (RRR.table[i][1][1]>=i)
		drawLink(g,i,RRR.table[i][1][1],1,2,draw);
	    }	    
	    
	    // Tm neighbour:
	    if(RRR.table[i][2][2]!=1){
		if (RRR.table[i][2][1]>i)
		    drawLink(g,i,RRR.table[i][2][1],2,1,draw);
	    }
	    // S neighbour:
	    if(RRR.table[i][3][2]!=1){
		if (RRR.table[i][3][1]>=i)
		drawLink(g,i,RRR.table[i][3][1],3,3,draw);
	    }
	}
    }
    

    private void drawLink(Graphics g, int i, int j, int M, int Mj, String draw){

	int x1,y1,x2,y2;
	

	
	int dotsize=10;

	g.translate(x0,-y0);

	x1 = (int)(RRR.reps[i].Imx[M-1]*(double)scale)+translist[i]*scale;
	double Y=(RRR.reps[i].Imy[M-1]*(double)scale);
	y1 = Height-(int)Y;

	x2 = (int)(RRR.reps[j].Imx[Mj-1]*(double)scale)+translist[j]*scale;
	Y=(RRR.reps[j].Imy[Mj-1]*(double)scale);
	y2 = Height-(int)Y;

	int x3 = (x1+x2)/2;
	int y3 = Math.min(y1,y2) - Math.abs(x1-x2)/8;

	drawCurve(g,x1,x2,x3,y1,y2,y3,draw);

	g.translate(-x0,y0);
    }


    private void drawCurve(Graphics g,
			  int X1, int X2, int X3, int Y1, int Y2, int Y3,String draw){

	double x1,x2,x3,y1,y2,y3,r,x,y;
	double A3,L1,L2,L3,B,C,T;
	int X, Y, R,a,b,D;

	if (X1<X2) {
	    x1 = (double) (X1);
	    x2 = (double) (X2);
	    y1 = (double) (Y1);
	    y2 = (double) (Y2);}
	else {
	    x1 = (double) (X2);
	    x2 = (double) (X1);
	    y1 = (double) (Y2);
	    y2 = (double) (Y1);}
	
	    
	x3 = (double) (X3);
	y3 = (double) (Y3);

	L1 = Math.sqrt((x2-x3)*(x2-x3)+(y2-y3)*(y2-y3));
	L2 = Math.sqrt((x1-x3)*(x1-x3)+(y1-y3)*(y1-y3));
	L3 = Math.sqrt((x1-x2)*(x1-x2)+(y1-y2)*(y1-y2));

	A3 = Math.abs(Math.acos((-L3*L3 + L1*L1 + L2*L2)/2/L1/L2));

	r = L3/Math.sin(A3)/2;
	
	T = (Math.PI-A3)-Math.atan((y1-y2)/(x1-x2));

	x = x1 + r*Math.sin(T);
	y = y1 + r*Math.cos(T);
	

	D = (int) (2*r);
	X = (int) (x-r);
	Y = (int) (y-r);

	int dotsize = 2;
	
	a = (int)(180*A3/Math.PI*2-360);
	b = (int)(180*T/Math.PI+90);

	g.setColor(Color.white);
	g.setXORMode(Color.black);
	g.drawArc(X,Y,D,D,b,a);
	//	if (draw=="draw") g.setColor(Color.green);
	//else g.setColor(colour1);
	g.fillOval(X3-dotsize,Y3-dotsize,2*dotsize,2*dotsize);
	//if (draw=="draw") g.setColor(Color.black);
	//else g.setColor(colour1);
	//g.drawOval(X3-dotsize,Y3-dotsize,2*dotsize,2*dotsize);
    }



    public void drawdots(Color colour){

	Graphics g = getGraphics();	
	int l = RRR.length();
	int i,x,y;		
	Font font;
	font = new Font("Serif", Font.BOLD, 14 );
	g.setFont(font);
	g.setColor(Color.red);
        g.drawString("EDIT MODE", 20, Height-70);

	for (i=0;i<l;i++){
	    // T neighbour:	    
	    if(RRR.table[i][1][2]!=1){
		drawDot(g,i,1,colour);
	    }	    
	    
	    // Tm neighbour:
	    if(RRR.table[i][2][2]!=1){
		drawDot(g,i,2,colour);
	    }
	    // S neighbour:
	    if(RRR.table[i][3][2]!=1){
		drawDot(g,i,3,colour);
	    }
	}
    }
    
    private void drawDot(Graphics g, int i, int M, Color colour){
	int x,y;

	g.translate(x0,-y0);
	g.translate(translist[i]*scale,0);
	g.setColor(colour);
	x = (int)(RRR.reps[i].Imx[M-1]*(double)scale);
	double Y=(RRR.reps[i].Imy[M-1]*(double)scale);
	y = Height-(int)Y;
	int dotsize = (int)Math.min(5,Y*0.3);
	g.fillOval(x-dotsize,y-dotsize,2*dotsize,2*dotsize);	
	if (colour!=Color.white) g.setColor(Color.black);
	g.drawOval(x-dotsize,y-dotsize,2*dotsize,2*dotsize);
	g.translate(-translist[i]*scale,0);
	g.translate(-x0,y0);
    }

    public void paint(Graphics g) {
	screenclear=true;
	update(g);

    }

    public void setcolour1(Color col1){
	colour1 = col1;
	repaint();
    }

    public void setcolour2(Color col2){
	colour2 = col2;
	repaint();
    }

    public void redraw(int newN, int newM) {

	oldN = N;
	oldM = M;
	if (newN<=0) newN = 1;
	if (newM<=0) newM = 1;

	trianglemode=false;
	RepList SSS = new RepList(grouptype,grouptype2);
	SSS.makeup(newN,newM);
	screenclear=true;

	RRR = SSS;
	N = newN;
	M = newM;
	repaint();
	
    }
    
    public void nextgroup(int dN){
	RepList SSS = new RepList(grouptype,grouptype2);
	trianglemode=false;
	oldN = N;
	N = N + dN;
	screenclear=true;
	if (N==0){totalclear=true; N=1;}
	SSS.makeup(N,M);	
	RRR = SSS;
	repaint();
    }	


    public void nextgroupM(int dM){
	RepList SSS = new RepList(grouptype,grouptype2);
	trianglemode=false;
	oldM = M;
	M = M + dM;
	screenclear=true;
	if (M==0){totalclear=true; M=1;}
	SSS.makeup(N,M);	
	RRR = SSS;
	repaint();
    }	


    public void clearall(){
	RepList SSS = new RepList(grouptype,grouptype2); 
	RRR = SSS;
	screenclear=true;
	totalclear=true;
	repaint();
    }
	
    public void changescale(int newScale){
	oldscale=scale;
	scale=newScale;
	screenclear=true;
	repaint();
    }

    public void setgrouptype(String typeofgroup){
	oldgrouptype=grouptype;	
	grouptype=typeofgroup;	
	screenclear=true;
	trianglemode=false;
	redraw(N,M);
    }

    public void setgrouptype2(String typeofgroup){
	oldgrouptype2=grouptype;	
	grouptype2=typeofgroup;	
	screenclear=true;
	trianglemode=false;
	redraw(N,M);
    }


    public void changex0(int dx){
	x0= x0+dx;
	screenclear=true;
	repaint();
    }


    public void changex0(float center){
	x0= Width/2  - (int) (center*((float)scale));
	screenclear=true;
	repaint();
    }


    public void changex0andScale(float center, int newscale){
	oldscale = scale;
	scale = newscale;
	x0= Width/2  - (int) (center*((float)scale));
	screenclear=true;
	repaint();
    }

	    

    public void setnewtriangle(boolean newtri){
	if (newtri==false) newtriangles=false;
	else newtriangles=true;
	trianglemode=true;
    }

    public void onetriangle(int A, int B, int C, int D) {
	int l;

	if (trianglemode==false){RepList SSS = new RepList(grouptype, grouptype2); RRR = SSS;}
	trianglemode=true;
	if (newtriangles==false) screenclear=true;
	
	l = RRR.length();
	if (newtriangles==false) l=l-1;
	
	if (A*D-B*C==0) {A=1;B=0;C=0;D=1;}
	ConjClassRep M = new  ConjClassRep(A,B,C,D);	
	RRR.reps[l] = M;
	RRR.table[l][0][0]=1;

	repaint();
	
    }


    public void onetriangle(int i, Color colour){
	
	Graphics g = getGraphics();
	HypTriangle HT = RRR.reps[i].tri(Height,Width,scale);
		    
	HT.find_Poly(scale,Height,r.width);
	Polylist[i]=HT.Poly;
	translist[i]=HT.translation;
	
	g.translate(x0,-y0);
	g.translate(translist[i]*scale,0);

	g.setColor(colour);
	g.fillPolygon(Polylist[i]);
	if (colour!=Color.white) g.setColor(colour2);
	//   if ((colour==Color.black)||(colour2==Color.black))
	//g.setColor(Color.yellow);}
	g.drawPolygon(Polylist[i]);
	g.translate(-translist[i]*scale,0);
	g.translate(-x0,y0);
    }
    


    public void onetriangle(String Mat) {
	int l;
	IntMat thisMat = new IntMat(); 
	IntMat nextMat = new IntMat(); 
	    

	if (trianglemode==false){
	    RepList SSS = new RepList(grouptype,grouptype2); 
	    RRR = SSS;}
	trianglemode=true;
	//	if (newtriangles==false)
	screenclear=true;
	
	l = RRR.length();
	thisMat = (IntMat) RRR.reps[l-1];
	
	if (Mat=="MT") nextMat= thisMat.mult(IntMat.T);
	if (Mat=="MTm") nextMat= thisMat.mult(IntMat.Tm);
	if (Mat=="MS") nextMat= thisMat.mult(IntMat.S);
	if (Mat=="MR") nextMat= thisMat.mult(IntMat.R);

	if (Mat=="TM") nextMat= IntMat.T.mult(thisMat);
	if (Mat=="TmM") nextMat= IntMat.Tm.mult(thisMat);
	if (Mat=="SM") nextMat= IntMat.S.mult(thisMat);
	if (Mat=="RM") nextMat= IntMat.R.mult(thisMat);

	if (newtriangles==false) l=l-1;

      	ConjClassRep M = new  ConjClassRep(nextMat);	
	
	RRR.reps[l] = M;
	RRR.table[l][0][0]=1;
	
	repaint();
	
    }


    public void mouseDragged(MouseEvent e) {
	Graphics g = getGraphics();
	g.setXORMode(Color.white);
	g.drawPolygon(Xcords,Ycords,4);	
	int x = e.getX();
	int y = e.getY();
	Xcords[2]=x; 	Xcords[3]=x;
	Ycords[1]=y; 	Ycords[2]=y;
	g.drawPolygon(Xcords,Ycords,4);
    }

    public void mouseMoved(MouseEvent e) {
    }

    public void mousePressed(MouseEvent e) {
	int i;
	if (is_a_rectangle){
	    Graphics g = getGraphics();
	    g.setXORMode(Color.white);
	    g.drawPolygon(Xcords,Ycords,4);}
	    
	last_x = e.getX();
	last_y = e.getY();
	for (i=0;i<4;i++){Xcords[i]=last_x;}
	for (i=0;i<4;i++){Ycords[i]=last_y;}
	is_a_rectangle = true;
    }

    public void mouseReleased(MouseEvent e) {
	end_x = e.getX();
	end_y = e.getY();
    }

    public void mouseEntered(MouseEvent e) {
    }

    public void mouseExited(MouseEvent e) {
    }

    public void mouseClicked(MouseEvent e) {
	if (!trianglemode){
	    Graphics g = getGraphics();		
	    int xa = e.getX();
	    int ya = e.getY();
	    int x=0;
	    int y=0;
	    int X,Y;
	    Polygon P;
	    int i=-1;
	    int dot = 5;	       
	    int j=0;
	    int l = RRR.length();
	    boolean found=false;
	    boolean dotfound=false;
	    while((i<l-1) && !found && !dotfound){
		i++;
		P = Polylist[i];//RRR.reps[i].tri(r.height,Width,scale);
		x = xa - translist[i]*scale - x0;
		y = ya + y0;
		if (P.contains(x,y)) found = true;
		j=0;
		while (j<3 && !dotfound){
		    j++;
		    if(RRR.table[i][j][2]==0){
			X = x-(int)(RRR.reps[i].Imx[j-1]*(double)scale);
			Y = Height-y-(int)(RRR.reps[i].Imy[j-1]*(double)scale);
			if ((X<= dot) && (Y<=dot)
			    &&(X>= -dot) && (Y>=-dot))  dotfound=true;
		    }
		}
	    }
	    if (dotfound && editmode) found=false;
		
	    if (found){
		int H = Height;
		IntMat Mat = new IntMat();
		Mat = (IntMat) RRR.reps[i];
		int sx = 580; 
		int sy = H-22;
		g.setColor(Color.white);
		g.fillRect(sx-29,H-50,200,50);
		g.setColor(Color.blue);
		drawMatrix(g,Mat,sx, sy);

		g.translate(x0,-y0);
		g.translate(translist[i]*scale,0);
		g.setXORMode(Color.black);
		g.fillPolygon(Polylist[i]);
		g.translate(-translist[i]*scale,0);
		g.translate(-x0,y0);
	    }
	    
	    else if (dotfound  && editmode) {

		if (linkmode) drawlinks("draw");
		drawdots(Color.white);

		ConjClassRep T  = new ConjClassRep(IntMat.T);
		ConjClassRep Tm  = new ConjClassRep(IntMat.Tm);
		ConjClassRep S  = new ConjClassRep(IntMat.S);
		
		int k=0,jn=0;  // jn of reps[next] corresponds to j of reps[i]
		int kn=0;

		//		RRR.table[i][j][2]=1;

		int next = RRR.table[i][j][1]; // this is the number of the 
		//                         // triangle we're going to move.
		
		//ConjClassRep oldTri = new conjClassRep(RRR.reps[next]);
		onetriangle(i,colour1);
		onetriangle(next,Color.white);
		

		if (j==1){
		    RRR.reps[next] = new ConjClassRep(RRR.reps[i].mult(T));
		    jn=2;
		}
		else if (j==2){
		    RRR.reps[next] = new ConjClassRep(RRR.reps[i].mult(Tm));
		    jn=1;
		}
		else {
		    RRR.reps[next] = new ConjClassRep(RRR.reps[i].mult(S));
		    jn=3;
		}
		
		ConjClassRep Con;

		// here we check up on connections of neigbours of the
		// triangle
		int wasNextTo;
		for (k=1;k<=3;k++){
		    //		    if (k!=jn){
			wasNextTo = RRR.table[next][k][1];
			if (k==1) kn=2;
			else if (k==2) kn=1; 
			else  kn=3;
			if (RRR.table[next][k][2]==1){
			    RRR.table[next][k][2]=0;
			    RRR.table[wasNextTo][kn][2]=0;
			}
			else {
			    if (k==1)
				Con = new ConjClassRep(RRR.reps[next].mult(T));
			    else if (k==2)
				Con =new ConjClassRep(RRR.reps[next].mult(Tm));
			    else
				Con = new ConjClassRep(RRR.reps[next].mult(S));
			    
	 		    if (Con.fracEqual(RRR.reps[wasNextTo])){
				RRR.table[wasNextTo][kn][2]=1;
				RRR.table[next][k][2]=1;
			    }
 			    else RRR.table[next][k][2]=0;
			    if (k==3 && wasNextTo==next) 		
				RRR.table[wasNextTo][k][2]=0;
			}
			onetriangle(wasNextTo,colour1);
			//}
		}

		onetriangle(next, colour1);
		if (linkmode) drawlinks("draw");
		drawdots(Color.yellow);
	    }
	}
    }
    
}


class DomainControls extends Panel
                  implements ActionListener, ItemListener
{

    private GridBagConstraints gbconstraints;
    private GridBagLayout gbLayout;

    private Choice group_choice;
    private Choice group_choice2;
    private TextField N;
    private TextField M;
    private TextField S;
    private Font font;
    private Font fontT;
    private UpperHalfPlane upperplane;
    private static Color cream     = new Color(206,255,204);
    private static Color orange_cream  = new Color(255,204,128);
    private static Color pale_blue     = new Color(221,238,255);
    Button d1;
    Button d2;
    Button e1=null;
    Button e2=null;   


    public DomainControls(UpperHalfPlane upperplane) {
	font = new Font("Serif", Font.BOLD, 12 );
	fontT = new Font("sanSerif", Font.PLAIN, 10 );
	Button a = null;
	Button b = null;
	Button c = null;
	Label lab1, lab2, lab3, lab4, lab5, lab6;
	group_choice = new Choice();
	group_choice2 = new Choice();



	this.upperplane = upperplane;

	gbLayout = new GridBagLayout();
	this.setLayout(gbLayout);
	gbconstraints = new GridBagConstraints();
	gbconstraints.fill = GridBagConstraints.HORIZONTAL;
	this.setBackground(pale_blue);

	newButton(a,0,0,1,1,"<--",orange_cream,fontT);
	newButton(a,1,0,1,1,"<-",orange_cream,fontT);
	newButton(a,2,0,1,1,"0",orange_cream,fontT);
	newButton(a,3,0,1,1,"->",orange_cream,fontT);
	newButton(a,4,0,1,1,"-->",orange_cream,fontT);


	setconstraints(5,0,1,2);
	gbconstraints.anchor =  GridBagConstraints.CENTER;

   littlelable words = new littlelable("Fundamental","Domains",40,140,35,60);
	words.setBounds(10,-10,150,40); 

	gbLayout.setConstraints(words,gbconstraints);
	add(words);



	String[] gpchoice = {"Gamma_0(N)","Gamma_1(N)",
			     "Gamma^0(N)","Gamma^1(N)","Gamma(N)"};
	newChoice(group_choice,5,gpchoice,cream,font);
	setconstraints(6,0,1,1);
	group_choice = new Choice();       
	gbLayout.setConstraints(group_choice,gbconstraints);
	newChoice(group_choice,5,gpchoice,cream,font);
	//	group_choice2.setFont(font);
	add(group_choice);


	String[] gpchoice2 = {"Gamma_0(M)","Gamma_1(M)",
			     "Gamma^0(M)","Gamma^1(M)","Gamma(M)"};
	setconstraints(6,1,1,1);
	group_choice2 = new Choice();       
	gbLayout.setConstraints(group_choice2,gbconstraints);
	newChoice(group_choice2,5,gpchoice2,cream,font);
	//	group_choice2.setFont(font);
	add(group_choice2);


	setconstraints(7,0,1,1);
	lab5 = new Label("  Set N:");
	lab5.setFont(font);
	gbLayout.setConstraints(lab5,gbconstraints);
	add(lab5);


	setconstraints(7,1,1,1);
	lab5 = new Label("  Set M:");
	lab5.setFont(font);
	gbLayout.setConstraints(lab5,gbconstraints);
	add(lab5);
	
	d1 = new Button("<");
	newButton2(d1,8,0,1,1,"<",cream,fontT);
	d2 = new Button(">");
	newButton2(d2,9,0,1,1,">",cream,fontT);
	e1 = new Button("<");
	newButton2(e1,8,1,1,1,"<",cream,fontT);
	e2 = new Button(">");
	newButton2(e2,9,1,1,1,">",cream,fontT);



	N = new TextField(" "+1, 4);
	newText(N,     10,0,1,1);

	M = new TextField(" "+1, 4);
	newText(M,     10,1,1,1);

	newButton(b,11,0,1,1,"Draw",cream,font);
	newButton(b,12,0,1,1,"edit",cream,font);
	newButton(b,13,0,1,1,"links",cream,font);
    }


    private void setconstraints (int X, int Y, int W, int H){
	gbconstraints.gridx = X;
	gbconstraints.gridy = Y;
	gbconstraints.gridwidth = W;
	gbconstraints.gridheight = H;
    }
    
    private void newText(TextField text, int X, int Y, int W, int H){
	setconstraints(X,Y,W,H);
	gbLayout.setConstraints(text,gbconstraints);
	text.addActionListener(this);
	add(text);
    }


    private void newChoice(Choice choice1, int n, String[] choiceList,
			   Color colour, Font font){

	choice1.addItemListener(this);
	int i;
	for(i=0;i<n;i++){
	    choice1.addItem(choiceList[i]);
	}

	choice1.setFont(font);
	choice1.setForeground(Color.black);
	choice1.setBackground(colour);
    }

    private void newButton(Button b, int X, int Y, int W, int H, String words,
			   Color colour, Font font){
	
	setconstraints(X,Y,W,H);
	b = new Button(words);
	b.setFont(font);
	b.setBackground(colour);
	gbLayout.setConstraints(b,gbconstraints);	
	b.addActionListener(this);
	add(b);
    }


    private void newButton2(Button b, int X, int Y, int W, int H, String words,
			   Color colour, Font font){
	
	setconstraints(X,Y,W,H);
	b.setFont(font);
	b.setBackground(colour);
	gbLayout.setConstraints(b,gbconstraints);	
	b.addActionListener(this);
	add(b);
    }


    public void actionPerformed(ActionEvent ev) {
	String label = ev.getActionCommand();
	if(label == "Draw") {
	    upperplane.redraw(Integer.parseInt(N.getText().trim()),
			      Integer.parseInt(M.getText().trim()));
	    N.setText(""+upperplane.N);
	    M.setText(""+upperplane.M);
		}

	else if(label == "edit") {
	    if (!upperplane.trianglemode){
	    if (upperplane.editmode) {
		upperplane.editmode=false;
		upperplane.drawdots(Color.white);
		upperplane.repaint(upperplane.N);
	    }
	    else {
		upperplane.editmode=true;
		upperplane.drawdots(Color.yellow);
	    }
	    }}

	else if(label == "links") {
	    if (!upperplane.trianglemode){
	    if (upperplane.linkmode) {
		upperplane.linkmode=false;
		upperplane.drawlinks("remove");
		//		upperplane.repaint(upperplane.N);
	    }
	    else {
		upperplane.linkmode=true;
		upperplane.drawlinks("draw");
	    }
	    if (upperplane.editmode) upperplane.drawdots(Color.yellow);
	    }
	}

	else if(label == "<-") {upperplane.changex0(-10);}

	else if(label == "->") {upperplane.changex0(10);}

	else if(label == "0") {upperplane.changex0((float)0);}

	else if(label == "-->") {upperplane.changex0(100);}

	else if(label == "<--") {upperplane.changex0(-100);}

	else if(ev.getSource() == d2){
	    //	    if(label == ">") {
		upperplane.nextgroup(1);
		N.setText(""+upperplane.N);
	}
	
	else if(ev.getSource() == d1){	    
	    //    else if(label == "<") {
	    upperplane.nextgroup(-1);
	    N.setText(""+upperplane.N);
	}
    

	else if(ev.getSource() == e2){
	    //	    if(label == ">") {
		upperplane.nextgroupM(1);
		M.setText(""+upperplane.M);
	}
	    
	else if(ev.getSource() == e1){	    
	    //   else if(label == "<") {
		upperplane.nextgroupM(-1);
		M.setText(""+upperplane.M);
	}
	

    }

    public void itemStateChanged(ItemEvent e) {
	String choice = (String) e.getItem();
	if (e.getSource()==group_choice) upperplane.setgrouptype(choice);
	else if(e.getSource()==group_choice2) upperplane.setgrouptype2(choice);
    }
    
}





class ColourControls extends Panel implements ItemListener, ActionListener {

    private UpperHalfPlane target;
    private Color  colour2;
    private TextField  A;
    private TextField  B;
    private TextField  C;
    private TextField  D;
    private TextField  numer;
    private TextField  denom;
    private TextField  Scaletext;
    private Choice colour_choice2;
    private Choice triangle_choice;
    private GridBagConstraints gbconstraints;
    private GridBagLayout gbLayout;

    private Color  colour1; 
    private Choice colour_choice1;

    private static Color green_cream  = new Color(227,255,128);
    private static Color orange_cream  = new Color(255,204,128);
    private static Color pale_purple  = new Color(238,221,255);
    private static Color mauve     = new Color(200,0,150);
    private static Color baby_blue = new Color(190,190,255);
    private static Color turquoise = new Color(70,220,200);
    private static Color orange    = new Color(250,155,10);
    private static Color apple     = new Color(190,250,130);
    private static Color banana    = new Color(255,220,0);
    private static Color chocolate  = new Color(120,50,50);
    private static Color grey = new Color(240,240,240);
    private static Color red = new Color(255,0,0);
    private static Color black = new Color(0,0,0);

    private String colChoices[]={"red","grey","mauve","baby blue","turquoise",
	  		       "orange","apple","banana","chocolate","black",
	  		       "random"};
    private Color colList[] = {red,grey,mauve,baby_blue,turquoise,
	  		       orange,apple,banana,chocolate,black};


    private String colChoices2[] = {"black","red","mauve","baby blue",
				  "turquoise",
				  "orange","apple","banana","chocolate","grey",
				  "random"};
    private Color colList2[] = {black,red,mauve,baby_blue,turquoise,
		      orange,apple,banana,chocolate,grey};



    public ColourControls(UpperHalfPlane target) {
	this.target = target;
	
	Font font;
	Font fontT;

	font = new Font("Serif", Font.BOLD, 12 );
	fontT = new Font("sanSerif", Font.PLAIN, 10 );	
	Button go =new Button();
	Button b =new Button();
	Button c =new Button();


	gbLayout = new GridBagLayout();
	this.setLayout(gbLayout);
	gbconstraints = new GridBagConstraints();
	gbconstraints.fill = GridBagConstraints.HORIZONTAL;


	this.setBackground(pale_purple);

	triangle_choice = new Choice();
	triangle_choice.addItemListener(this);
	triangle_choice.setFont(font);
	triangle_choice.addItem("move");
	triangle_choice.addItem("copy");


	Button expandrectangle =new Button();
	Button resetscalebut =new Button();
	Button Scalechange =new Button();

	  Button MId =new Button();
	//Button  =new Button();
	//Button  =new Button();
	newButton(c, 0, 0, 4, 1, "clear screen",orange_cream, font);
	newButton(resetscalebut,   0, 1, 4, 1, "reset scale = 50",
		  orange_cream, font);
	newButton(expandrectangle, 0, 2, 4, 1, "expand rectangle",
		  orange_cream, font);
	newButton(Scalechange, 0, 3, 2, 1, "Scale",orange_cream, font);

	Scaletext = new TextField("50", 3);
	newText(Scaletext,     2, 3, 3, 1);


	setconstraints(0,4,2,1);
	Label cspace = new Label("outline:");
	cspace.setFont(font);
	gbLayout.setConstraints(cspace,gbconstraints);
	add(cspace);

	setconstraints(2,4,3,1);
	colour_choice2 = new Choice();       
	gbLayout.setConstraints(colour_choice2,gbconstraints);
	newChoice(colour_choice2,11,colChoices2,orange_cream,font);
	colour_choice2.setFont(font);
	add(colour_choice2);


	setconstraints(0,5,2,1);
	cspace = new Label("filling:");
	cspace.setFont(font);
	gbLayout.setConstraints(cspace,gbconstraints);
	add(cspace);

	colour_choice1 = new Choice();

	newChoice(colour_choice1,11,colChoices,orange_cream,font);

	setconstraints(2,5,3,1);
	gbLayout.setConstraints(colour_choice1,gbconstraints);
	colour_choice1.setFont(font);
	add(colour_choice1);


	setconstraints(0,7,6,2);
	gbconstraints.ipady = 10;
	gbconstraints.ipadx = 10;
	gbconstraints.anchor =  GridBagConstraints.CENTER;
	littlelable divideline = new 
	    littlelable("Hyperbolic","Triangles",45,140,35,60);
	divideline.setBounds(10,-10,150,45);	
	gbLayout.setConstraints(divideline,gbconstraints);
	add(divideline);
	gbconstraints.ipadx = 0;
	gbconstraints.ipady = 0;

	
	setconstraints(0,10,3,1);
	Label LM = new Label("Matrix: ");
	gbLayout.setConstraints(LM,gbconstraints);
	add(LM);

	setconstraints(0,11,1,1);
	Label L3 = new Label("M=");
	gbLayout.setConstraints(L3,gbconstraints);
	add(L3);

	setconstraints(1,11,1,1);
	A = new TextField("1", 2);
	gbLayout.setConstraints(A,gbconstraints);
	add(A);

	setconstraints(2,11,1,1);
	B = new TextField("0", 2);
	gbLayout.setConstraints(B,gbconstraints);
	add(B);

	newButton(MId, 3, 11, 2, 1, "M=Id",green_cream, font);

	setconstraints(1,12,1,1);
	C = new TextField("0", 2);
	gbLayout.setConstraints(C,gbconstraints);
	add(C);

	setconstraints(2,12,1,1);
	D = new TextField("1", 2);
	gbLayout.setConstraints(D,gbconstraints);
	add(D);

	newButton(b, 0, 13, 3, 1, "Draw Triangle",green_cream, font);

	setconstraints(3,13,2,1);
	triangle_choice.setBackground(green_cream);
	gbLayout.setConstraints(triangle_choice,gbconstraints);
	add(triangle_choice);

	Button mt = null;
	newButton(mt, 0, 14, 1, 1, "MT'",green_cream, fontT);
	newButton(mt, 1, 14, 1, 1, "MS",green_cream, fontT);
	newButton(mt, 2, 14, 1, 1, "MT",green_cream, fontT);
	newButton(mt, 3, 14, 1, 1, "MR",green_cream, fontT);

	newButton(mt, 0, 15, 1, 1, "T'M",green_cream, fontT);
	newButton(mt, 1, 15, 1, 1, "SM",green_cream, fontT);
	newButton(mt, 2, 15, 1, 1, "TM",green_cream, fontT);
	newButton(mt, 3, 15, 1, 1, "RM",green_cream, fontT);

	newButton(go, 0, 16, 2, 1, "Go to",green_cream, font);
	newButton(go, 2, 16, 3, 1, "Scale to",green_cream, font);

	setconstraints(0,16,3,1);
	gbconstraints.ipady = 10;
	gbconstraints.ipadx = 10;
 
    }

    private void setconstraints (int X, int Y, int W, int H){
	gbconstraints.gridx = X;
	gbconstraints.gridy = Y;
	gbconstraints.gridwidth = W;
	gbconstraints.gridheight = H;
    }

    private void newChoice(Choice choice1, int n, String[] choiceList,
			   Color colour, Font font){

	//choice1 = new Choice();
	choice1.addItemListener(this);
	int i;
	for(i=0;i<n;i++){
	    choice1.addItem(choiceList[i]);
	}

	choice1.setFont(font);
	choice1.setForeground(Color.black);
	choice1.setBackground(colour);
    }


    private void newText(TextField text, int X, int Y, int W, int H){
	setconstraints(X,Y,W,H);
	gbLayout.setConstraints(text,gbconstraints);
	text.addActionListener(this);
	add(text);
    }


    private void newButton(Button b, int X, int Y, int W, int H, String words,
			   Color colour, Font font){
	
	setconstraints(X,Y,W,H);
	b = new Button(words);
	b.setFont(font);
	b.setBackground(colour);
	gbLayout.setConstraints(b,gbconstraints);	
	b.addActionListener(this);
	add(b);
    }


    public void itemStateChanged(ItemEvent e) {

	Random ran = new Random();
	String choice = (String) e.getItem();
	int i=0;
	boolean found = false;

	if (e.getSource() == colour_choice1){	    
	   if (choice.equals("random")) {
	       target.setcolour1(new Color(ran.nextFloat(),ran.nextFloat(),ran.nextFloat()));}
	   else while(!found && i<10){
	       if (choice.equals(colChoices[i])) {
		   found = true;
		   target.setcolour1(colList[i]);}
	   i++;}

	}
    

	else if (e.getSource() == colour_choice2){	    

	   if (choice.equals("random")) {
	       target.setcolour2(new Color(ran.nextFloat(),ran.nextFloat(),ran.nextFloat()));}
	   else while(!found && i<10){
	       if (choice.equals(colChoices2[i])) {
		   found = true;
		   target.setcolour2(colList2[i]);}
	   i++;}
	}
	
	else if (e.getSource() == triangle_choice){
	    //	    String choice = (String) e.getItem();
	    if (choice.equals("move")) 	target.setnewtriangle(false);
	    else  target.setnewtriangle(true);
	}
    }
    

    public void actionPerformed(ActionEvent ev) {
	String label = ev.getActionCommand();
	if (label=="Draw Triangle"){
	    int a = Integer.parseInt(A.getText().trim());
	    int b = Integer.parseInt(B.getText().trim());
	    int c = Integer.parseInt(C.getText().trim());
	    int d = Integer.parseInt(D.getText().trim());
	    if (a*d-b*c==1)  target.onetriangle(a,b,c,d);
	    else {
		A.setText(""+1);
		B.setText(""+0);
		C.setText(""+0);
		D.setText(""+1);
	    }	     
	}


	else if (label=="Scale"){
	    int newscale = Integer.parseInt(Scaletext.getText().trim());
	    if (newscale > 200000000) 
		{         newscale = 200000000;
		Scaletext.setText(""+200000000);}
	    float r = (float) target.getBounds().width;
	    float X0 = (float) target.x0;
	    float oldscale = (float) target.scale;
	    float centerpoint = (r/2 - X0)/oldscale;
	target.changex0andScale(centerpoint,newscale);
	}


	else if (label=="reset scale = 50"){
	    int newscale = 50;
	    float r = (float) target.getBounds().width;
	    float X0 = (float) target.x0;
	    float oldscale = (float) target.scale;
	    float centerpoint = (r/2 - X0)/oldscale;
	    target.changex0andScale(centerpoint,50);
	    Scaletext.setText(""+50);
	}

	else if (label=="M=Id"){
	target.onetriangle(1,0,0,1);
	A.setText(""+1);
	B.setText(""+0);
	C.setText(""+0);
	D.setText(""+1);
	}



	else if (label=="Go to"){
	    float centerpoint = 0;
	    IntMat M = (IntMat) target.RRR.reps[target.Replength-1];
	    if (M.c!=0) centerpoint = ((float) M.a)/((float) M.c);
	    else centerpoint = ((float) M.b)/((float) M.d);
	    target.changex0(centerpoint);}    


	else if (label=="expand rectangle"){
	    float Ht= (float) target.getBounds().height;
	    float xa= (float) target.last_x;
	    float xb= (float) target.end_x;
	    float ya= Ht - (float) Math.min(target.last_y,target.end_y);
	    float ymin= Ht - (float) Math.max(target.last_y,target.end_y);
	    float yb= (float) target.y0;
	    float X0= (float) target.x0;
	    float sc= (float) target.scale;
	    if (ya > yb && target.is_a_rectangle && ymin < yb) {
		float centerpoint = (xb+xa - X0*2)/2/sc;
		float newscalefloat = (Ht-yb)*sc/(ya-yb);
		int newscale = (int) newscalefloat;
	    if (newscale > 200000000)  newscale = 200000000;
		Scaletext.setText(""+newscale);
		target.changex0andScale(centerpoint,newscale);}
	}



	else if (label=="Scale to"){
	    int newscale=50;
	    float centerpoint = 0;
	    IntMat M = (IntMat) target.RRR.reps[target.Replength-1];
 HypTriangle HyT = new HypTriangle(target.Height,target.Width,target.scale,M);

	    if (M.c==0) {
		newscale = 200; //Math.max(target.scale,200);
		centerpoint = ((float) M.b)/((float) M.d);}
	    else {
		newscale =  Math.min((int) (150/HyT.MaxY),200000000);
		centerpoint = ((float) M.a)/((float) M.c);} 
	    Scaletext.setText(""+newscale);
	    target.changex0andScale(centerpoint,newscale);
	}    



	else {

	boolean modewas = target.trianglemode;

	if (label=="clear screen"){
	    int newscale = 50;
	    target.changescale(50);
	    Scaletext.setText(""+50);
	    target.changex0((float)0);
	    target.clearall();
	    modewas = false;
	}

	else {
	if (label=="MT'"){
	target.onetriangle("MTm");
	}

	else if (label=="T'M"){
	target.onetriangle("TmM");
	}

	else  target.onetriangle(label);
	}

	if (modewas==true){
	A.setText(""+target.RRR.reps[target.Replength-1].a);
	B.setText(""+target.RRR.reps[target.Replength-1].b);
	C.setText(""+target.RRR.reps[target.Replength-1].c);
	D.setText(""+target.RRR.reps[target.Replength-1].d);
	}
	else 
	    {
	A.setText(""+target.RRR.reps[0].a);
	B.setText(""+target.RRR.reps[0].b);
	C.setText(""+target.RRR.reps[0].c);
	D.setText(""+target.RRR.reps[0].d);
	    }

	}

    }

}

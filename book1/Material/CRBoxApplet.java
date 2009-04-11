/*
 * Created on 2004-11-23
 *
 * To change the template for this generated file go to
 * Window&gt;Preferences&gt;Java&gt;Code Generation&gt;Code and Comments
 */
 
package RussianBox;

import java.util.*;
import javax.swing.*;
import java.awt.*;
import java.awt.event.*;

/**
 * @author larry, Liu
 *
 * To change the template for this generated type comment go to
 * Window&gt;Preferences&gt;Java&gt;Code Generation&gt;Code and Comments
 */

class RBConst{
	public static final int MAP_WIDTH	= 16;
	public static final int MAP_HEIGHT	= 20;
	public static final int MAP_BORDER	= 4;
	
	public static final int LEFT  =0;
	public static final int RIGHT =1;
	public static final int DOWN  =2;
	
	public static final Color boxColor[]={Color.RED, Color.BLUE, Color.GREEN, Color.YELLOW}; 
};

class RBox {
	private static final int ShapeType[][][] = 
	{ 
		{ 
			{ 1, 1, 1, 1 }, 
			{ 0, 0, 0, 0 }, 
			{ 0, 0, 0, 0 }, 
			{ 0, 0, 0, 0 }
		}, 
		{
			{ 1, 0, 0, 0 }, 
			{ 1, 1, 1, 0 }, 
			{ 0, 0, 0, 0 }, 
			{ 0, 0, 0, 0 }
		}, 
		{
			{ 1, 1, 0, 0 }, 
			{ 0, 1, 1, 0 }, 
			{ 0, 0, 0, 0 }, 
			{ 0, 0, 0, 0 }
		}, 
		{
			{ 1, 1, 0, 0 }, 
			{ 1, 1, 0, 0 }, 
			{ 0, 0, 0, 0 }, 
			{ 0, 0, 0, 0 }
		} 
	};
	
	//members
	private int type;
	public  int shape[][];
	public  int x;				//box coordinate
	public  int y;

	private RBox(int iType) {
		type  = iType;
		shape = new int[4][4];

		for (int i = 0; i < 4; ++i) {
			for (int j = 0; j < 4; ++j) {
				shape[i][j] = ShapeType[iType][i][j];
			}
		}

		x = 0;	
		y = 0;
	}

	//static factory method
	public static RBox getNextRBox(int iType) {
		return new RBox(iType);
	}

	public Color getColor(){
		return RBConst.boxColor[type];
	}
		
	public boolean turnBox(int map[][]) {
		// 1. can turn?
		int temp[][] = new int[4][4];
		for (int i = 0; i < 4; ++i) {
			for (int j = 0; j < 4; ++j) {
				temp[j][4 - 1 - i] = shape[i][j];
			}
		}

		for (int i = 0; i < 4; ++i) {
			for (int j = 0; j < 4; ++j) {
				if ((temp[i][j] != 0) && 
					(map[y + i+RBConst.MAP_BORDER][x + j+RBConst.MAP_BORDER] != 0)) {
					return false; //can not turn;
				}
			}
		}

		//2. turn!
		for (int i = 0; i < 4; ++i) {
			for (int j = 0; j < 4; ++j) {
				shape[i][j] = temp[i][j];
			}
		}
		return true;
	}

	public boolean moveBox(int dir, int map[][]) {
		int dx[] = { -1, 1, 0 };
		int dy[] = { 0, 0, 1 };
		
		//1. Can move?
		for (int i = 0; i < 4; ++i) {
			for (int j = 0; j < 4; ++j) {
				if((shape[i][j] != 0) && 
				   (map[y+dy[dir]+i+RBConst.MAP_BORDER][x+dx[dir]+j+RBConst.MAP_BORDER] != 0)){
					return false;		//can not move
				}
			}
		}
		
		//2. move!
		x+=dx[dir];
		y+=dy[dir];
		
		return true;
	}
};

class BoxController extends Thread {
	//member vars
	private RBox currentBox = null;			//composite pattern
	private Random boxRand = new Random();
	private JPanel panel;
	private int map[][];
	private int gndTimeOut;

	public boolean stop = true;
	
	//ctor
	public BoxController(JPanel argPanel, int argMap[][]) {
		panel=argPanel;
		map=argMap;
	}
	
	public RBox getCurrentBox(){
		return currentBox;
	}

	public synchronized boolean stopStartController() {
		stop = !stop;
		if(!stop){
			start();
		}
		return stop;
	}
	
	private void touchGround(){
		if(currentBox.y<RBConst.MAP_BORDER){
			stop=true;		//game over
		}
		
		//1. freeze this box and set related map-cell to 1
		for(int i=0; i<4; ++i){
			for(int j=0; j<4; ++j){
				if(currentBox.shape[i][j] !=0 ){
					map[currentBox.y+i+RBConst.MAP_BORDER][currentBox.x+j+RBConst.MAP_BORDER]=1;
				}
			}
		}
		currentBox=null;
				
		//2. check if can delete the last 4 lines
		for(int i=RBConst.MAP_HEIGHT-1; i>0; --i){
			int cellCount=0;
			int nullCount=0;
			for(int j=0; j<RBConst.MAP_WIDTH; ++j){
				if(map[i+RBConst.MAP_BORDER][j+RBConst.MAP_BORDER] ==1){
					cellCount++;
				}
				else if(map[i+RBConst.MAP_BORDER][j+RBConst.MAP_BORDER] ==1){
					nullCount++;
				}
			}
			if(nullCount == RBConst.MAP_WIDTH){
				break;
			}
			if(cellCount == RBConst.MAP_WIDTH){
				removeRow(i);
			}
		}
	}

	public void run() {
		while (!stop) {
			//1. check if there is a box currently
			if (currentBox == null) {
				currentBox = RBox.getNextRBox(Math.abs(boxRand.nextInt()) % 4);
				gndTimeOut=0;
			}

			//2. drop the current box
			if(!currentBox.moveBox(RBConst.DOWN, map)){
				if(gndTimeOut++ >=3){
					touchGround();
				}
			}
			
			try {
				sleep(500);
			} catch (InterruptedException e) {
				throw new RuntimeException(e);
			}
			
			panel.repaint();
		}//end of while
	}
	
	//proxy pattern
	public void moveBox(int dir){
		if(currentBox != null){
			currentBox.moveBox(dir, map);
		}
	}
	
	public void turnBox(){
		if(currentBox != null){
			currentBox.turnBox(map);
		}
	}
	
	private void removeRow(int row){
		for(int i=row; i>0; --i){
			int nullCount=0;
			for(int j=0; j<RBConst.MAP_WIDTH; ++j){
				if(map[i+RBConst.MAP_BORDER][j+RBConst.MAP_BORDER] ==0){
					nullCount++;
				}
				map[i+RBConst.MAP_BORDER][j+RBConst.MAP_BORDER] = map[i+RBConst.MAP_BORDER-1][j+RBConst.MAP_BORDER];
			}
			if(nullCount == RBConst.MAP_WIDTH){
				return;
			}
		}				
	}
};

class MainPanel extends JPanel {
	//member vars
	private int map[][]=new int[RBConst.MAP_HEIGHT+
								RBConst.MAP_BORDER*2]
							   [RBConst.MAP_WIDTH+
							    RBConst.MAP_BORDER*2];

	private BoxController controller;
	
	//ctor
	public MainPanel() {
		super();
		initMap();
		controller = new BoxController(this, map);
	}

	public BoxController getController() {
		return controller;
	}

	private void initMap() {
		//set all map to -1
		for (int i = 0; i < RBConst.MAP_HEIGHT+RBConst.MAP_BORDER*2; ++i) {
			for (int j = 0; j < RBConst.MAP_WIDTH+RBConst.MAP_BORDER*2; ++j) {
				map[i][j] = -1;
			}
		}
		
		//set main space to 0, which will left border as -1
		for (int i = 0; i < RBConst.MAP_HEIGHT; ++i) {
			for (int j = 0; j < RBConst.MAP_WIDTH; ++j) {
				map[i+RBConst.MAP_BORDER][j+RBConst.MAP_BORDER] = 0;
			}
		}	
	}

	//	paint method overrided.
	public void paintComponent(Graphics g) {
		super.paintComponent(g);

		int maxWidth  = getWidth();
		int maxHeight = getHeight();
		int unitWid	  = maxHeight/RBConst.MAP_HEIGHT;

		//1. draw map.
		for(int i =0; i<RBConst.MAP_HEIGHT; i++){
			for(int j=0; j<RBConst.MAP_WIDTH; j++){
				g.setColor(Color.BLACK);
				g.drawRect(j*unitWid, i*unitWid, unitWid, unitWid);
				if(map[i+RBConst.MAP_BORDER][j+RBConst.MAP_BORDER] == 1){
					g.setColor(Color.ORANGE);
					g.fillRect(j*unitWid, i*unitWid, unitWid, unitWid);
				}
			}
		}
		
		//2. draw current box
		RBox box=controller.getCurrentBox();
		if(box == null){
			return;
		}
		
		for(int i=0; i<4; ++i){
			for(int j=0; j<4; ++j){
				if(box.shape[i][j] != 0){
					g.setColor(box.getColor());
					g.fillRect((box.x+j)*unitWid, (box.y+i)*unitWid,
							   unitWid, unitWid);
				}
			}
		}
	}
};

public class CRBoxApplet extends JApplet {
	private MainPanel mainPanel = new MainPanel();
	private JButton stopBtn = new JButton("start");

	private class StopBtnListener implements ActionListener {
		public void actionPerformed(ActionEvent e) {
			if (mainPanel.getController() != null) {
				if(mainPanel.getController().stopStartController()){
					stopBtn.setText("stop");
				}
				else{
					stopBtn.setText("start");
				}
			}
		}
	};
	private StopBtnListener stopBtnListener = new StopBtnListener();
	
	class CKeyListener implements KeyListener {
		public void keyPressed(KeyEvent e) {
			//System.out.println("keyPressed: "+e.paramString());
			if(e.getKeyCode()==KeyEvent.VK_DOWN){
				mainPanel.getController().moveBox(RBConst.DOWN);
			}
			if(e.getKeyCode()==KeyEvent.VK_LEFT){
				mainPanel.getController().moveBox(RBConst.LEFT);
			}
			if(e.getKeyCode()==KeyEvent.VK_RIGHT){
				mainPanel.getController().moveBox(RBConst.RIGHT);
			}
			if(e.getKeyCode()==KeyEvent.VK_UP){
				mainPanel.getController().turnBox();
			}
		}
		public void keyReleased(KeyEvent e) {
			//System.out.println("keyReleased: "+e.paramString());
		}
		public void keyTyped(KeyEvent e) {
			//System.out.println("keyTyped: "+e.paramString());
		}
	};
	CKeyListener keyListener = new CKeyListener();

	public void init() {
		Container cp = getContentPane();
		cp.add(BorderLayout.CENTER, mainPanel);
		cp.add(BorderLayout.SOUTH, stopBtn);
		stopBtn.addKeyListener(keyListener);
		stopBtn.addActionListener(stopBtnListener);
	}

	public static void main(String[] args) {
		JApplet applet = new CRBoxApplet();
		JFrame  frame  = new JFrame("Russian Box");
		frame.getContentPane().add(applet);
		frame.setSize(800, 600);
		applet.init();
		applet.start();
		frame.setVisible(true);
	}
};

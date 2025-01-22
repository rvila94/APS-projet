/* ========================================================================== */
/* == UPMC/master/info/4I506 -- Janvier 2016/2017/2018                     == */
/* == SU/FSI/master/info/MU4IN503 -- Janvier 2020/2021/2022                == */
/* == Analyse des programmes et sémantiques                                == */
/* ========================================================================== */
/* == hello-APS Syntaxe JAVA                                               == */
/* == Fichier: AstApp.java                                                 == */
/* == Arbre de syntaxe abstraite (programmes)                              == */
/* ========================================================================== */

import java.util.ArrayList;

public class AstProg implements Ast {

    ArrayList<Ast> cmds;
    
    AstProg(ArrayList<Ast> cs) {
	this.cmds = cs;
    }
    
    public String toPrologString() {
	String r = "";
	int n = cmds.size()-1;
	r = "prog([";
	
	for(int i=0; i < n; i++)
	    r += cmds.get(i).toPrologString()+",";
	r += cmds.get(n).toPrologString();
	r += "])";
	return r;
    }

}

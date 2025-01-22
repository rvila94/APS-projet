/* ========================================================================== */
/* == UPMC/master/info/4I506 -- Janvier 2016/2017/2018                     == */
/* == SU/FSI/master/info/MU4IN503 -- Janvier 2020/2021/2022                == */
/* == Analyse des programmes et sémantiques                                == */
/* ========================================================================== */
/* == hello-APS Syntaxe JAVA                                               == */
/* == Fichier: AstApp.java                                                 == */
/* == Arbre de syntaxe abstraite (applications)                            == */
/* ========================================================================== */

import java.util.ArrayList;

public class AstEcho implements Ast {

    Ast expr;
    
    AstEcho(Ast e) {
	this.expr = e;
    }

    //@Override
    public String toPrologString() {
	String r = "echo("+expr.toPrologString()+")";
	return r;
    }

}

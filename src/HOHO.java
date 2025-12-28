package src;
import java.nio.file.*;
import java.util.*;
import java.io.IOException;

public class HOHO {

  public static void main(String[] args) throws IOException {
    String source;
    if (args.length == 0) {
      source = """
        x = 2;
        HOHO x;

        MERRY (x < 3) {
          HOHO 111;
        } JINGLE {
          HOHO 999;
        }

        SLEIGH i FROM 0 TO 4 {
          HOHO i;
        }
      """;
      System.out.println("Running built-in demo program:\\n" + source);
    } else {
      source = Files.readString(Path.of(args[0]));
    }

    Lexer lexer = new Lexer(source);
    List<Token> tokens = lexer.lex();

    Parser parser = new Parser(tokens);
    List<Stmt> program = parser.parseProgram();

    Interpreter interpreter = new Interpreter();
    interpreter.run(program);
  }

  enum TokenType {
    HOHO,       // print
    MERRY,      // if
    JINGLE,     // else
    SLEIGH,     // for
    FROM, TO,

    L_PAREN, R_PAREN,
    L_BRACE, R_BRACE,
    SEMI,
    ASSIGN,

    PLUS, MINUS, STAR, SLASH,
    EQEQ, NEQ, LT, LTE, GT, GTE,

    NUMBER,
    IDENT,
    STRING,


    EOF
  }

  record Token(TokenType type, String lexeme, int pos) {}

  static class Lexer {
    private final String s;
    private int i = 0;

    Lexer(String s) { this.s = s; }

    List<Token> lex() {
      List<Token> out = new ArrayList<>();
      while (!isAtEnd()) {
        skipWhitespace();
        if (isAtEnd()) break;
        int start = i;
        char c = peek();

        if (Character.isDigit(c)) {
          while (!isAtEnd() && Character.isDigit(peek())) advance();
          out.add(new Token(TokenType.NUMBER, s.substring(start, i), start));
          continue;
        }

        if (Character.isLetter(c) || c == '_') {
          while (!isAtEnd() && (Character.isLetterOrDigit(peek()) || peek() == '_')) advance();
          String word = s.substring(start, i);
          out.add(new Token(keywordType(word), word, start));
          continue;
        }

if (c == '"') {
    advance(); 
    int startt = i;

    while (!isAtEnd() && peek() != '"') {
        advance();
    }

    if (isAtEnd()) {
        throw error("Unterminated string", startt);
    }

    String value = s.substring(startt, i);
    advance(); 
    out.add(new Token(TokenType.STRING, value, startt));
    continue;
}



        switch (c) {
          case '(' -> { advance(); out.add(new Token(TokenType.L_PAREN, "(", start)); }
          case ')' -> { advance(); out.add(new Token(TokenType.R_PAREN, ")", start)); }
          case '{' -> { advance(); out.add(new Token(TokenType.L_BRACE, "{", start)); }
          case '}' -> { advance(); out.add(new Token(TokenType.R_BRACE, "}", start)); }
          case ';' -> { advance(); out.add(new Token(TokenType.SEMI, ";", start)); }
          case '+' -> { advance(); out.add(new Token(TokenType.PLUS, "+", start)); }
          case '-' -> { advance(); out.add(new Token(TokenType.MINUS, "-", start)); }
          case '*' -> { advance(); out.add(new Token(TokenType.STAR, "*", start)); }
          case '/' -> { advance(); out.add(new Token(TokenType.SLASH, "/", start)); }
          case '=' -> {
            advance();
            if (!isAtEnd() && peek() == '=') { advance(); out.add(new Token(TokenType.EQEQ, "==", start)); }
            else out.add(new Token(TokenType.ASSIGN, "=", start));
          }
          case '!' -> {
            advance();
            if (!isAtEnd() && peek() == '=') { advance(); out.add(new Token(TokenType.NEQ, "!=", start)); }
            else throw error("Unexpected '!'. Did you mean '!='?", start);
          }
          case '<' -> {
            advance();
            if (!isAtEnd() && peek() == '=') { advance(); out.add(new Token(TokenType.LTE, "<=", start)); }
            else out.add(new Token(TokenType.LT, "<", start));
          }
          case '>' -> {
            advance();
            if (!isAtEnd() && peek() == '=') { advance(); out.add(new Token(TokenType.GTE, ">=", start)); }
            else out.add(new Token(TokenType.GT, ">", start));
          }
          default -> throw error("Unexpected character: " + c, start);
        }
      }
      out.add(new Token(TokenType.EOF, "", i));
      return out;
    }

    private TokenType keywordType(String w) {
      return switch (w) {
        case "HOHO" -> TokenType.HOHO;
        case "MERRY" -> TokenType.MERRY;
        case "JINGLE" -> TokenType.JINGLE;
        case "SLEIGH" -> TokenType.SLEIGH;
        case "FROM" -> TokenType.FROM;
        case "TO" -> TokenType.TO;
        default -> TokenType.IDENT;
      };
    }

    private void skipWhitespace() {
      while (!isAtEnd()) {
        char c = peek();
        if (c == ' ' || c == '\t' || c == '\r' || c == '\n') { advance(); continue; }
        if (c == '/' && i + 1 < s.length() && s.charAt(i + 1) == '/') {
          while (!isAtEnd() && peek() != '\n') advance();
          continue;
        }
        break;
      }
    }

    private boolean isAtEnd() { return i >= s.length(); }
    private char peek() { return s.charAt(i); }
    private void advance() { i++; }

    private RuntimeException error(String msg, int pos) {
      return new RuntimeException("Lexer error at pos " + pos + ": " + msg);
    }
  }

  interface Expr {}
  record Num(int value) implements Expr {}
  record Var(String name) implements Expr {}
  record Bin(Expr left, TokenType op, Expr right) implements Expr {}

  interface Stmt {}

  record PrintStmt(Expr expr) implements Stmt {}
  record AssignStmt(String name, Expr expr) implements Stmt {}
  record BlockStmt(List<Stmt> statements) implements Stmt {}
  record IfStmt(Expr cond, BlockStmt thenBlock, BlockStmt elseBlock) implements Stmt {}
  record ForStmt(String var, Expr from, Expr to, BlockStmt body) implements Stmt {}
  record Str(String value) implements Expr {}


  static class Parser {
    private final List<Token> t;
    private int p = 0;

    Parser(List<Token> tokens) { this.t = tokens; }

    List<Stmt> parseProgram() {
      List<Stmt> out = new ArrayList<>();
      while (!check(TokenType.EOF)) out.add(parseStmt());
      return out;
    }

    private Stmt parseStmt() {
      if (match(TokenType.HOHO)) {
        Expr e = parseExpr();
        consume(TokenType.SEMI, "Expected ';' after HOHO print.");
        return new PrintStmt(e);
      }

      if (match(TokenType.MERRY)) {
        consume(TokenType.L_PAREN, "Expected '(' after MERRY.");
        Expr cond = parseExpr();
        consume(TokenType.R_PAREN, "Expected ')' after condition.");
        BlockStmt thenB = parseBlock();
        BlockStmt elseB = null;
        if (match(TokenType.JINGLE)) elseB = parseBlock();
        return new IfStmt(cond, thenB, elseB);
      }

      if (match(TokenType.SLEIGH)) {
        Token name = consume(TokenType.IDENT, "Expected loop variable after SLEIGH.");
        consume(TokenType.FROM, "Expected FROM in SLEIGH loop.");
        Expr from = parseExpr();
        consume(TokenType.TO, "Expected TO in SLEIGH loop.");
        Expr to = parseExpr();
        BlockStmt body = parseBlock();
        return new ForStmt(name.lexeme(), from, to, body);
      }

      if (check(TokenType.IDENT) && checkNext(TokenType.ASSIGN)) {
        Token name = advance();
        consume(TokenType.ASSIGN, "Expected '='.");
        Expr e = parseExpr();
        consume(TokenType.SEMI, "Expected ';' after assignment.");
        return new AssignStmt(name.lexeme(), e);
      }

      throw error(peek(), "Unknown statement.");
    }

    private BlockStmt parseBlock() {
      consume(TokenType.L_BRACE, "Expected '{' to start block.");
      List<Stmt> stmts = new ArrayList<>();
      while (!check(TokenType.R_BRACE)) {
        if (check(TokenType.EOF)) throw error(peek(), "Unclosed block. Missing '}'.");
        stmts.add(parseStmt());
      }
      consume(TokenType.R_BRACE, "Expected '}' after block.");
      return new BlockStmt(stmts);
    }

    private Expr parseExpr() { return parseEquality(); }

    private Expr parseEquality() {
      Expr e = parseComparison();
      while (match(TokenType.EQEQ, TokenType.NEQ)) {
        TokenType op = previous().type();
        Expr r = parseComparison();
        e = new Bin(e, op, r);
      }
      return e;
    }

    private Expr parseComparison() {
      Expr e = parseTerm();
      while (match(TokenType.LT, TokenType.LTE, TokenType.GT, TokenType.GTE)) {
        TokenType op = previous().type();
        Expr r = parseTerm();
        e = new Bin(e, op, r);
      }
      return e;
    }

    private Expr parseTerm() {
      Expr e = parseFactor();
      while (match(TokenType.PLUS, TokenType.MINUS)) {
        TokenType op = previous().type();
        Expr r = parseFactor();
        e = new Bin(e, op, r);
      }
      return e;
    }

    private Expr parseFactor() {
      Expr e = parseUnary();
      while (match(TokenType.STAR, TokenType.SLASH)) {
        TokenType op = previous().type();
        Expr r = parseUnary();
        e = new Bin(e, op, r);
      }
      return e;
    }

    private Expr parseUnary() {
      if (match(TokenType.MINUS)) {
        Expr right = parseUnary();
        return new Bin(new Num(0), TokenType.MINUS, right);
      }
      return parsePrimary();
    }

    private Expr parsePrimary() {
      if (match(TokenType.NUMBER)) return new Num(Integer.parseInt(previous().lexeme()));
      if (match(TokenType.IDENT)) return new Var(previous().lexeme());
      if (match(TokenType.STRING))
    return new Str(previous().lexeme());

      if (match(TokenType.L_PAREN)) {
        Expr e = parseExpr();
        consume(TokenType.R_PAREN, "Expected ')' after expression.");
        return e;
      }

      throw error(peek(), "Expected expression.");
    }

    private boolean match(TokenType... types) {
      for (TokenType tt : types) {
        if (check(tt)) { advance(); return true; }
      }
      return false;
    }

    private Token consume(TokenType tt, String msg) {
      if (check(tt)) return advance();
      throw error(peek(), msg);
    }

    private boolean check(TokenType tt) { return peek().type() == tt; }
    private boolean checkNext(TokenType tt) {
      if (p + 1 >= t.size()) return false;
      return t.get(p + 1).type() == tt;
    }

    private Token advance() {
      if (!check(TokenType.EOF)) p++;
      return previous();
    }

    private Token peek() { return t.get(p); }
    private Token previous() { return t.get(p - 1); }

    private RuntimeException error(Token tok, String msg) {
      return new RuntimeException("Parser error at pos " + tok.pos() + " near '" + tok.lexeme() + "': " + msg);
    }
  }

  static class Interpreter {
    private final Map<String, Object> env = new HashMap<>();

    void run(List<Stmt> program) {
      for (Stmt s : program) exec(s);
    }

    private void exec(Stmt s) {
      if (s instanceof PrintStmt ps) {
        Object v = eval(ps.expr());
        System.out.println(v);
        return;
      }

      if (s instanceof AssignStmt as) {
        Object v = eval(as.expr());
        env.put(as.name(), v);
        return;
      }

      if (s instanceof BlockStmt bs) {
        for (Stmt st : bs.statements()) exec(st);
        return;
      }

      if (s instanceof IfStmt is) {
    Object condVal = eval(is.cond());
    if (!(condVal instanceof Integer))
        throw new RuntimeException("Condition must be a number");

    int cond = (Integer) condVal;

    if (cond != 0) exec(is.thenBlock());
    else if (is.elseBlock() != null) exec(is.elseBlock());
    return;
}


      if (s instanceof ForStmt fs) {
    Object fromVal = eval(fs.from());
    Object toVal = eval(fs.to());

    if (!(fromVal instanceof Integer) || !(toVal instanceof Integer))
        throw new RuntimeException("Loop bounds must be numbers");

    int from = (Integer) fromVal;
    int to = (Integer) toVal;

    for (int i = from; i <= to; i++) {
        env.put(fs.var(), i);
        exec(fs.body());
    }
    return;
}

      throw new RuntimeException("Unknown stmt: " + s);
    }

    private Object eval(Expr e) {

    if (e instanceof Num n) return n.value();

    if (e instanceof Str s) return s.value();

    if (e instanceof Var v) {
        if (!env.containsKey(v.name()))
            throw new RuntimeException("Undefined variable: " + v.name());
        return env.get(v.name());
    }

    if (e instanceof Bin b) {
        Object left = eval(b.left());
        Object right = eval(b.right());

        if (b.op() == TokenType.PLUS &&
            (left instanceof String || right instanceof String)) {
            return String.valueOf(left) + String.valueOf(right);
        }

        if (!(left instanceof Integer) || !(right instanceof Integer))
            throw new RuntimeException("Invalid operands for operator " + b.op());

        int L = (Integer) left;
        int R = (Integer) right;

        return switch (b.op()) {
            case PLUS -> L + R;
            case MINUS -> L - R;
            case STAR -> L * R;
            case SLASH -> {
                if (R == 0) throw new RuntimeException("Division by zero.");
                yield L / R;
            }
            case EQEQ -> (L == R) ? 1 : 0;
            case NEQ -> (L != R) ? 1 : 0;
            case LT -> (L < R) ? 1 : 0;
            case LTE -> (L <= R) ? 1 : 0;
            case GT -> (L > R) ? 1 : 0;
            case GTE -> (L >= R) ? 1 : 0;
            default -> throw new RuntimeException("Unknown operator: " + b.op());
        };
    }

    throw new RuntimeException("Unknown expr: " + e);
}

  }
}


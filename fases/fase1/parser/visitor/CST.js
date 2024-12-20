
// Auto-generated
import Node from './Node.js';

export class Productions extends Node {

    constructor(id, name, exprs) {
        super();
        this.id = id;
		this.name = name;
		this.exprs = exprs;
    }

    accept(visitor) {
        return visitor.visitProductions(this);
    }
}
    
export class Options extends Node {

    constructor(exprs) {
        super();
        this.exprs = exprs;
    }

    accept(visitor) {
        return visitor.visitOptions(this);
    }
}
    
export class Union extends Node {

    constructor(exprs) {
        super();
        this.exprs = exprs;
    }

    accept(visitor) {
        return visitor.visitUnion(this);
    }
}
    
export class Expression extends Node {

    constructor(label, exprs, count) {
        super();
        this.label = label;
		this.exprs = exprs;
		this.count = count;
    }

    accept(visitor) {
        return visitor.visitExpression(this);
    }
}
    
export class String extends Node {

    constructor(value, ignoresCase) {
        super();
        this.value = value;
		this.ignoresCase = ignoresCase;
    }

    accept(visitor) {
        return visitor.visitString(this);
    }
}
    
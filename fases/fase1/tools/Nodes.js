const nodes = {
   Producciones: ['id', 'expr', 'alias'],
   Opciones: ['exprs'],
   Union: ['exprs'],
   Expresion: ['expr', 'label', 'qty'],
   String: ['val', 'isCase'],
   Any: ['isAny'],
   Corchetes: ['exprs', 'isCase'],
   rango: ['start', 'end'],
   literalRango:['val'],
   idRel:['val'],
   grupo:['expr']
};

export default nodes;

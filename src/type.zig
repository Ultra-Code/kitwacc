const parser = @import("parser.zig");
const AstNode = parser.AstTree.AstNode;

const TypeKind = enum {
    TY_INT,
    TY_PTR,
};

pub const Type = struct {
    kind: TypeKind,
    base: ?*const Type = null,
    pub fn isInt(ty: Type) bool {
        return ty.kind == .TY_INT;
    }
    pub fn isPtr(ty: Type) bool {
        return ty.kind == .TY_PTR and ty.base != null;
    }
    fn toPointer(base: Type) Type {
        return Type{ .kind = .TY_PTR, .base = &base };
    }
};
pub const int_ty = Type{ .kind = .TY_INT };

pub fn addType(node: ?*AstNode) void {
    if (node == null or node.?.type != null) {
        return;
    }
    addType(node.?.lhs);

    addType(node.?.rhs);

    switch (node.?.value) {
        .no_value => {},
        .identifier => |_| {
            node.?.type = int_ty;
        },
        .number => |_| {
            node.?.type = int_ty;
        },
        .block => |blocks| {
            var it = blocks.iterator();
            while (it.next()) |next_node| {
                addType(next_node);
            }
        },
        .if_statement => |*if_| {
            addType(if_.if_expr);
            addType(if_.then_branch);
            addType(if_.else_branch);
        },
        .loop => |*loop| {
            addType(loop.init);
            addType(loop.condition);
            addType(loop.body);
            addType(loop.body);
        },
    }

    switch (node.?.kind) {
        .NK_ADD, .NK_SUB, .NK_MUL, .NK_DIV, .NK_ASSIGN => node.?.type = node.?.lhs.?.type,
        .NK_EQ, .NK_NE, .NK_LT, .NK_LE, .NK_GT, .NK_GE => node.?.type = int_ty,
        .NK_NEG => node.?.type = node.?.rhs.?.type,
        .NK_ADDR => node.?.type = node.?.rhs.?.type.?.toPointer(),
        .NK_DEREF => {
            if (node.?.rhs.?.type.?.isPtr()) {
                node.?.type = node.?.rhs.?.type.?.base.?.*;
            } else {
                node.?.type = int_ty;
            }
        },
        else => {},
    }
}

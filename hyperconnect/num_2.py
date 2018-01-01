class CategoryTree:

    def __init__(self):
        self.root = None
        self.tree = {}

    def add_category(self, category, parent):
        if parent is None:
            self.root = category
            self.tree[category] = (parent, [])
        else:
            if category in self.tree or parent not in self.tree:
                raise KeyError
            else:
                self.tree[parent][1].append(category)
                self.tree[category] = (parent, [])

    def get_children(self, parent):
        if parent in self.tree:
            return self.tree[parent][1]
        else:
            raise KeyError

c = CategoryTree()
c.add_category('A', None)
c.add_category('B', 'A')
c.add_category('C', 'A')
print(','.join(c.get_children('A') or []))
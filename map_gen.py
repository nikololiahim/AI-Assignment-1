from tkinter import *


class Matrix:
    def __init__(self, size):
        self.height = size[0]
        self.width = size[1]
        self.matrix = [[0]*size[1] for i in range(size[0])]

    def set(self, i, j, v):
        if -1 < i < self.height and -1 < j < self.width:
            self.matrix[i][j] = v
        else:
            raise IndexError("Indices are invalid!")



class MapButton(Button):
    texts = ['', 'O', 'H', 'T']

    def update_text(self):
        global w
        self.cur_type.set(
            (self.cur_type.get()+1) % len(self.texts)
        )
        next_type = self.texts[self.cur_type.get()]
        self.text.set(next_type)
        w.matrix.set(self.coords[0], self.coords[1], self.cur_type.get())

    def reset_text(self):
        self.cur_type.set(0)
        self.text.set("")

    def set_type(self, type):
        self.cur_type.set(type)
        self.text.set(self.texts[self.cur_type.get()])

    def __init__(self, coords: tuple, type: int, **kwargs):
        self.text = StringVar(master=kwargs['master'])
        self.cur_type = IntVar(master=kwargs['master'])
        self.cur_type.set(type)
        self.coords = coords
        self.type = type
        self.text.set(self.texts[type])
        super().__init__(textvariable=self.text,
                         command=self.update_text,
                         **kwargs)




class Window:

    _VERSION = "0.0"

    def copy_text_to_clipboard(self):
        field_value = self.text.get("1.0", 'END')  # get field value from event, but remove line return at end
        self.root.clipboard_clear()  # clear clipboard contents
        self.root.clipboard_append(field_value)  # append new value to clipbaord

    def __init__(self, root: Tk):
        self.root = root
        self.matrix = Matrix((20, 20))
        self._place_root()
        self._place_frames()
        self._place_buttons()
        self._place_textbox()
        self._place_export_to_predicates_chinese()
        self._place_export_to_predicates_normal()
        self.root.bind("<Control-Key-C>", lambda event: self.copy_text_to_clipboard())
        self._place_numbers()
        self._place_reset()
        self._place_import()

    def update_matrix(self, i, j):
        self.buttons[i][j].update_text()
        self.matrix.set(i, j, self.buttons[i][j].type)

    def _place_root(self):
        self.root.title("Mag Generator, v. " + self._VERSION)
        self.root.resizable(True, True)
        self.root.state('zoomed')

    def _place_frames(self):
        self.frames = [[Frame()] * 40 for _ in range(22)]
        # colors = ['black', 'white']
        for i in range(22):
            for j in range(40):
                self.frames[i][j] = Frame(self.root, background='lightblue', height=20, width=20)
                self.frames[i][j].grid(row=i, column=j, sticky=N + S + E + W)
                self.frames[i][j].bind("<Button-1>", lambda event: self.frames[i][j].focus_set())
                self.root.grid_columnconfigure(j, weight=1)
                self.root.grid_rowconfigure(i, weight=1)

    def _place_buttons(self):
        h, w = self.matrix.height, self.matrix.width
        self.buttons = [[0]*w for i in range(h)]
        for i in range(1, h+1):
            for j in range(1, w+1):
                self.buttons[i-1][j-1] = MapButton((i-1, j-1),
                                                   0,
                                                   master=self.frames[i][j])

                self.buttons[i-1][j-1].pack(expand=True, fill=BOTH)

    def _place_textbox(self):
        self.frames[1][22] = Frame(self.root, background='bisque', bd=5)
        self.frames[1][22].grid(column=22, row=1, sticky=N + S + E + W, columnspan=8, rowspan=8)
        self.text = Text(self.frames[1][22], wrap=WORD, width=12, height=12)
        self.text.pack(fill=BOTH, expand=True)

    def export_to_predicates_normal(self):
        res = []
        h, w = self.matrix.height, self.matrix.width
        for i in range(h):
            for j in range(w):
                cell = self.matrix.matrix[i][j]
                if cell == 2:
                    res.append(f'h({h-1-i}, {j}).')
                elif cell == 1:
                    res.append(f'o({h-i-1}, {j}).')
                elif cell == 3:
                    res.append(f't({h-i-1}, {j}).')
        res.sort(key=lambda x: x[0])
        self.text.delete(1.0, END)
        self.text.insert(1.0, '\n'.join(res))


    def export_to_predicates_chinese(self):
        res = []
        h, w = self.matrix.height, self.matrix.width
        for i in range(h):
            for j in range(w):
                cell = self.matrix.matrix[i][j]
                if cell == 2:
                    res.append(f'h({i}, {j}).')
                elif cell == 1:
                    res.append(f'o({i}, {j}).')
                elif cell == 3:
                    res.append(f't({i}, {j}).')
        res.sort(key=lambda x: x[0])
        self.text.delete(1.0, END)
        self.text.insert(1.0, '\n'.join(res))

    def _place_numbers(self):
        # for i in range(20):
        #     Label(master=self.frames[21][i+1], text=f"{i}",
        #           background="lightblue").pack(fill=BOTH, expand=True)
        #     Label(master=self.frames[i+1][0], text=f"{19-i}",
        #           background="lightblue").pack(fill=BOTH, expand=True)
        #
        for i in range(20):
            Label(master=self.frames[0][i+1], text=f"{i}",
                  background="lightblue").pack(fill=BOTH, expand=True)
            Label(master=self.frames[i+1][0], text=f"{i}",
                  background="lightblue").pack(fill=BOTH, expand=True)

    def _place_export_to_predicates_chinese(self):
        self.frames[10][22] = Frame(self.root, background='bisque', bd=5)
        self.frames[10][22].grid(column=22, row=10, sticky=N + S + E + W, columnspan=4, rowspan=2)
        self.export_to_pred_chinese = Button(self.frames[10][22],
                                             text="Export to Prolog\nPredicates\n(origin at top left)",
                                             width=6,
                                             height=3, command=self.export_to_predicates_chinese)
        self.export_to_pred_chinese.pack(fill=BOTH, expand=True)

    def _place_export_to_predicates_normal(self):
        self.frames[10][26] = Frame(self.root, background='bisque', bd=5)
        self.frames[10][26].grid(column=26, row=10, sticky=N + S + E + W, columnspan=4, rowspan=2)
        self.export_to_pred_normal = Button(self.frames[10][26],
                                             text="Export to Prolog\nPredicates\n(origin at bottom left)",
                                             width=6,
                                             height=3,
                                             command=self.export_to_predicates_normal)
        self.export_to_pred_normal.pack(fill=BOTH, expand=True)

    def reset(self):
        h, w = self.matrix.height, self.matrix.width
        for i in range(h):
            for j in range(w):
                self.matrix.matrix[i][j] = 0
                self.buttons[i][j].reset_text()

    def _place_reset(self):
        self.frames[1][31] = Frame(self.root, background='bisque', bd=5)
        self.frames[1][31].grid(column=31, row=1, sticky=N + S + E + W, columnspan=4, rowspan=2)
        self.reset_button = Button(self.frames[1][31],
                                            text="Reset",
                                            width=6,
                                            height=3,
                                            command=self.reset)
        self.reset_button.pack(fill=BOTH, expand=True)

    def import_from_pred(self):
        self.reset_button.invoke()
        t = self.text.get(1.0, END).split('\n')
        t.remove('')
        for i in range(len(t)):
            t[i] = (t[i][0] + ', ' + t[i][2:-2]).split(', ')
            I, J = int(t[i][1]), int(t[i][2])
            if t[i][0] == 'o':
                self.buttons[I][J].set_type(1)
                self.matrix.matrix[I][J] = 1
            elif t[i][0] == 'h':
                self.buttons[I][J].set_type(2)
                self.matrix.matrix[I][J] = 2
            elif t[i][0] == 't':
                self.buttons[I][J].set_type(3)
                self.matrix.matrix[I][J] = 3

    def _place_import(self):
        self.frames[4][31] = Frame(self.root, background='bisque', bd=5)
        self.frames[4][31].grid(column=31, row=4, sticky=N + S + E + W, columnspan=4, rowspan=2)
        self.import_button = Button(self.frames[4][31],
                                            text="Import from\nProlog predicates",
                                            width=6,
                                            height=3,
                                            command=self.import_from_pred)
        self.import_button.pack(fill=BOTH, expand=True)


w = Window(Tk())
w.root.mainloop()
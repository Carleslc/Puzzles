from PyQt5.QtWidgets import QWidget, QPushButton
from PyQt5.QtSvg import QSvgWidget

class Window(QWidget):

  def __init__(self, left = 100, top = 100, width = 500, height = 500):
    super().__init__()
    self.left = max(0, left)
    self.top = max(0, top)
    self.width = max(20, width)
    self.height = max(20, height)
    self.update_geometry()
    self.widgetSvg = QSvgWidget(parent=self)
    self.widgetSvg.setGeometry(10, 10, self.width - 20, self.height - 20)
  
  def update_geometry(self):
    self.setGeometry(self.left, self.top, self.width, self.height)
  
  def display_svg(self, svg):
    self.widgetSvg.load(svg)
    self.repaint()

  def add_button(self, text, on_click):
    btn = QPushButton(text, self)
    self.height = self.height + 60
    self.update_geometry()
    btn.move(int(self.width / 2) - 50, self.height - 50)
    btn.clicked.connect(on_click)

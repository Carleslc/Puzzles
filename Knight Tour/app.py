from window import Window

from PyQt5.QtWidgets import QApplication

class App():

  def __init__(self):
    self.app = QApplication([])
    self.window = Window()
  
  def exec(self):
    self.window.show()
    self.app.exec()

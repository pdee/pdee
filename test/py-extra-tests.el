;;; py-extra-tests.el --- extra tests                -*- lexical-binding: t; -*-

;; URL: https://gitlab.com/python-mode-devs
;; Keywords: convenience

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

;; (setq py-verbose-p t)

(require 'org)
(org-babel-do-load-languages
     'org-babel-load-languages
     '((emacs-lisp . t)
       (python . t)
       ))

(require 'py-setup-ert-tests)

(ert-deftest py-ert-moves-up-execute-statement-test-RdqUKX ()
  (py-test-point-min
   "print(\"I'm the py-execute-statement-test\")"
   'python-mode
   'py-verbose-p
   (cond ((executable-find "python2")
          (progn
            (goto-char (point-min))
            (let ((py-shell-name "python2"))
              (py-execute-statement)
              (sit-for 0.1)
              (set-buffer (get-buffer "*Python2*"))
              (goto-char (point-max))
              (and (should (search-backward "py-execute-statement-test" nil t 1))
	           (py-kill-buffer-unconditional (current-buffer))))))
         ((executable-find "python3")
          (progn
            (goto-char (point-min))
            (let ((py-shell-name "python3"))
              (py-execute-statement)
              (sit-for 0.1)
              (set-buffer (get-buffer "*Python3*"))
              (goto-char (point-max))
              (and (should (search-backward "py-execute-statement-test" nil t 1))
	           (py-kill-buffer-unconditional (current-buffer)))))))
   (when py-debug-p (message "py-ert-moves-up-execute-statement-test-RdqUKX: %s" "Can't see python2"))))

(ert-deftest UnicodeEncodeError-lp-550661-test-1oxvP0 ()
  (py-test
   "#! /usr/bin/env python3
print(u'\\xA9')"
   'python-mode
   'py-verbose-p
   (let ((py-return-result-p t)
	 (py-store-result-p t))
     (goto-char (point-max))
     (py-execute-buffer)
     ;; (setq erg (car (read-from-string py-result)))
     ;; (message "UnicodeEncodeError-lp-550661-test-1 erg: %s" erg)
     (sit-for 0.1)
     (should (string= "©" py-result)))))

(ert-deftest py-describe-symbol-fails-on-modules-lp-919719-test-9UErj2 ()
  (py-test
      "#! /usr/bin/env python
# -*- coding: utf-8 -*-
import os
os.write"
    'python-mode
    'py-verbose-p
    (if (executable-find "python")
        (progn
          (goto-char (point-max))
          (forward-char -1)
          (py-help-at-point)
          (sit-for 0.1)
          (set-buffer py-output-buffer)
          (goto-char (point-max))
          (when py-debug-p (switch-to-buffer (current-buffer)))
          (when py-debug-p (message "%s" (current-buffer)))
          (goto-char comint-last-output-start)
          (sit-for 0.1)
          (should (string-match "write" (buffer-substring-no-properties (point) (point-max)))))
      (when py-verbose-p (message "py-describe-symbol-fails-on-modules-lp-919719-test-9UErj2: %s" "No executable python found")))))

(ert-deftest py-describe-symbol-fails-on-modules-lp-919719-test-MppJiJ ()
  (py-test
      "#! /usr/bin/env python3
# -*- coding: utf-8 -*-
import os
os.write"
    'python-mode
    'py-verbose-p
    (if (executable-find "python3")
        (progn
          (goto-char (point-max))
          (forward-char -1)
          (py-help-at-point)
          ;; (sit-for 0.1)
          (set-buffer py-output-buffer)
          (switch-to-buffer (current-buffer)) 
          (goto-char (point-max))
          (when py-debug-p (switch-to-buffer (current-buffer)))
          (when py-debug-p (message "%s" (current-buffer)))
          (goto-char (point-min))
          (should (string-match "write" (buffer-substring-no-properties (point) (point-max)))))
      (when py-verbose-p (message "py-describe-symbol-fails-on-modules-lp-919719-test-9UErj2: %s" "No executable python found")))))

(ert-deftest py-ert-execute-block-fast-9Ui5ja-zo3sa5 ()
  (py-test-point-min
   "try:
    a
except:
    NameError
    a=1
finally:
    a+=1
    print(a)"
   'python-mode
   'py-verbose-p
   (let ((py-fast-process-p t)
	 (py-return-result-p t)
	 ;; (py-debug-p t)
	 py-result py-split-window-on-execute)
     (py-execute-block)
     (sit-for 0.3)
     (when py-debug-p (message "py-ert-execute-block-fast-9Ui5ja, py-result: %s" py-result))
     (should (string-match "[0-9]" py-result)))))

(ert-deftest py-ert-execute-block-9Ui5ja-DUvXA6 ()
  (py-test-point-min
   "try:
    a
except:
    NameError
    a=1
finally:
    a+=1
    print(a)"
   'python-mode
   'py-verbose-p
   (setq py-result "")
   (let ((py-fast-process-p nil)
	 (py-return-result-p t)
	 ;; (py-debug-p t)
	 py-split-window-on-execute)
     (py-execute-block)
     (sit-for 0.1)
     (when py-debug-p (message "py-ert-execute-block-fast-9Ui5ja, py-result: %s" py-result))
     (should (string-match "[0-9]+" py-result)))))

(ert-deftest py-ert-moves-up-execute-statement-python3-dedicated-test-zI51W7 ()
  (py-test-point-min
   "print(\"I'm the py-execute-statement-python3-dedicated-test\")"
   'python-mode
   'py-verbose-p
   (let (;; (py-debug-p t)
	 py-store-result-p
	 erg)
     (call-interactively 'py-execute-statement-python3-dedicated)
     ;; (sit-for 0.1 t)
     (set-buffer py-output-buffer)
     ;; (switch-to-buffer (current-buffer))
     (goto-char (point-min))
     (should (search-forward "py-execute-statement-python3-dedicated-test" nil t 1)))))

(ert-deftest py-ert-execute-statement-fast-7XrRee ()
  (py-test-point-min
   "print(2)"
   'python-mode
   'py-verbose-p
   (let ((py-fast-process-p t)
	 (py-return-result-p t)
	 py-result py-store-result-p)
     (py-execute-statement-fast)
     (sit-for 0.1)
     (should (string= "2" py-result)))))

;; adapted from python.el
(ert-deftest py-syntax-after-backspace-TwyMwn-xjlPqf ()
  (py-test
   "\"\""
   'python-mode
   'py-verbose-p
   (goto-char (point-max))
   (should (string= (buffer-string) "\"\""))
   (should (null (nth 3 (parse-partial-sexp (point-min) (point)))))))

(ert-deftest py-ert-execute-statement-fast-test-noYr4j ()
  (py-test-point-min
   "print(123234)"
   'python-mode
   'py-verbose-p
   (goto-char (point-min))
   (let ((py-split-window-on-execute py-switch-buffers-on-execute-p)
         (shell (py-choose-shell)))
     ;; delete old buffer if existing
     ;; (py-kill-buffer-unconditional  (concat "*" (capitalize shell) " Fast*"))
     (py-execute-statement-fast)
     (set-buffer (concat "*" (capitalize shell) " Fast*"))
     (sit-for 0.1)
     (goto-char (point-max))
     (when py-verbose-p (message "py-ert-execute-statement-fast-test: current-buffer: %s" (current-buffer)))
     (should (search-backward "123234")))))

(ert-deftest py-ert-fast-complete-vS8fnm ()
  (py-test
   "obj"
   'python-mode
   'py-verbose-p
   (goto-char (point-max))
   (py-fast-complete)
   (sit-for 0.1)
   (goto-char (point-max))
   (when py-debug-p (message "py-ert-fast-complete-1, current-buffer: %s" (current-buffer)))
   (should (search-backward "ect"))))

(ert-deftest py-execute-string-text-dtOWbA1 ()
  (py-test
   ""
   'python-mode
   'py-verbose-p
   (let ((py-store-result-p t))
     (py-execute-string "print(\"foo\")" nil t)
     (should (string= py-result "foo")))))

(ert-deftest py-ert-class-definitions-lp-1018164-test-3pDuRq ()
  (py-test
   "#! /usr/bin/env python
# -*- coding: utf-8 -*-
import sys
import io
import subprocess
import re
import copy
import pickle
import textwrap
import datetime
import util
import cldef
import datatypes
from themes.thutil import lisp, fontNames, Color, emacsPosition
###############################################################################
version = 3
###############################################################################
# used to raise a type error if a call is made with incorrect arguments
def _emptyParametersCheck(): pass
###############################################################################
class Font(object, metaclass=cldef.metaClass):
    __cldef = cldef.metaClass.wrapclass()
    __cldef.slots(\"__fontStr\")
    __cldef.fields(\"weight slant width height pointSize size face\")
    __cldef.field(\"mapKey\", initfunc=lambda f:(f.size,f.weight,f.slant))
    __cldef.field(\"sortKey\", initfunc=lambda f:(f.width,f.height))
    ###########################################################################
    __parseFontStrRE = __cldef.constant(re.compile(
        r'-outline-Courier New-(?P<weight>bold|normal)'\\
        r'-(?P<slant>i|r)-normal-normal' \\
        r'-(?P<height>\\d+)-(?P<size>\\d+)-\\d+-\\d+-c-(?P<width>\\d+)-iso10646-1'))
    ###########################################################################
    def __init__(self, fontStr):
        self.__fontStr = fontStr
        match = self.__parseFontStrRE.match(fontStr)
        self.weight = match.group('weight')
        self.slant = 'italic' if match.group('slant') == 'i' else 'regular'
        self.width = int(match.group('width'))//10
        self.height = int(match.group('height'))
        self.pointSize = (int(match.group('size')) + 5)//10
        self.size = \"%sx%s\" % (self.__width, self.__height)
        self.face = \"Courier New\"
    ###########################################################################
    def __str__(self): return self.__fontStr
    ###########################################################################
    def qt(self):
        if 'QtGui' not in globals():
            global QtGui
            from PyQt4 import QtGui
        font = QtGui.QFont(self.face, self.pointSize)
        font.setItalic(self.slant == 'italic')
        font.setBold(self.weight == 'bold')
        return font
###############################################################################
class FontAttrs(object, metaclass=cldef.metaClass):
    __cldef = cldef.metaClass.wrapclass()
    mapKey = property(lambda self: (self.size, self.weight, self.slant))
    ###########################################################################
    class FontAttrs(__cldef.FieldClass):
        access, fields = 'rw', \"fontSize fontWeight fontSlant\"
        def sethook(obj, fieldName, value):
            value = str(value)
            if value in obj.fontOptions(fieldName):
                return value
            raise ValueError(\"%s invalid value for %s\"%(value, fieldName))
    ###########################################################################
    attrNames = __cldef.constant(frozenset(FontAttrs.fields.split()))
    ###########################################################################
    class FontAttrAbbrevs(__cldef.FieldClass):
        access, fields = 'rw', tuple(\"size weight slant\".split())
        def fget(obj): pass
        def gethook(obj,name,\*p):
            return getattr(obj, 'font'+name.capitalize())
        def fset(obj,val): pass
        def sethook(obj,name,val,\*p):
            setattr(obj, 'font'+name.capitalize(), val)
    ###########################################################################
    def __init__(self, size='8x13', weight='bold', slant='regular'):
        self.__initAttrs(\*\*locals())
    ###########################################################################
    def font():
        # font options contain the valid values for each font attr
        _fontOptions = {'weight':('normal','bold'),
                        'slant':('regular','italic')}
        # Get the list of font names from thutil and create the font lookup
        # dict. It is assumed that the weight and slant are not specified,
        # so will fill in the \"typical\" options here.
        _fontMap = dict()
        for fontStr in fontNames():
            for weight in ('normal', 'bold'):
                for slant in ('r', 'i'):
                    font = Font(fontStr.replace(\"\*-\*\", weight+'-'+slant))
                    _fontMap[font.mapKey] = font
        # getFont: use the fontAttrSize, fontWeight and fontSize attrs
        # to lookup the font in _fontMap
        def getFont(self): return _fontMap[self.mapKey]
        # scan all the values of _fontMap an garther all fontSize options
        _fontSizes = set(f.size for f in _fontMap.values())
        def fontSizeSortKey(size):
            return tuple(int(i) for i in size.split('x'))
        _fontOptions['size'] = tuple(sorted(_fontSizes, key=fontSizeSortKey))
        #provide a function to query the fontoptions
        def fontOptions(attrName):
            attrName = attrName.lower()
            if attrName.startswith('font'):
                attrName = attrName[len('font'):]
            return _fontOptions[attrName]
        return property(fget=getFont), staticmethod(fontOptions)
    font, fontOptions = font()
###############################################################################
class ColorAttrs(object, metaclass=cldef.metaClass):
    __cldef = cldef.metaClass.wrapclass()
    attrNames = \"foregroundColor backgroundColor cursorColor\".split()
    attrNames = __cldef.constant(tuple(attrNames))
    __cldef.fields(attrNames.val, 'rw', sethook=Color)
    ###########################################################################
    def __init__(self, backgroundColor=\"grey13\", foregroundColor='navajowhite',
                 cursorColor=None):
        self.backgroundColor = backgroundColor
        self.foregroundColor = foregroundColor
        self.cursorColor = self.getColor_i(cursorColor,\"light green\", \"black\")
    ###########################################################################
    def getColor_i(self, val, forDark, forLight):
        if val is None:
            val = forDark if self.backgroundColor.isDark() else forLight
        return Color(val)
###############################################################################
class Face(object, metaclass=cldef.metaClass):
    ###########################################################################
    __cldef = cldef.metaClass.wrapclass(privAttrAccess=True)
    unspecified = __cldef.constant(None)
    __cldef.fields('name theme')
    __cldef.fields('inherit', access='rw',
                   sethook=lambda obj,val: obj.setInheritHook_i(val))
    ###########################################################################
    class FontAttrs(__cldef.FieldClass):
        access, fields = 'rw', (\"bold\",\"italic\",\"underline\",\"raised\")
        sethook = lambda val: val if val is None else bool(val)
        gethook = lambda obj,attr,val: obj.getAttrHook_i(attr, val)
    ###########################################################################
    class ColorAttrs(__cldef.FieldClass):
        access, fields = 'rw', (\"foreground\", \"background\")
        sethook = lambda val: val if val is None else Color(val)
        gethook = lambda obj,attr,val: obj.getAttrHook_i(attr, val)
    ###########################################################################
    fontAttrs, colorAttrs, faceAttrs = __cldef.constants(
        frozenset(FontAttrs.fields),
        frozenset(ColorAttrs.fields),
        frozenset(FontAttrs.fields + ColorAttrs.fields + ('inherit',)))
    ###########################################################################
    def __init__(self, name, theme, \*\*opts):
        self.name, self.theme = name, theme
        for attr in self.faceAttrs:
            self.__setPrivAttr(attr,None)
        for attr,val in opts.items():
            setattr(self, attr, val)
    ###########################################################################
    def __str__(self):
        return self.setFaceSexpr()
    ###########################################################################
    def copy(self):
        copy = self.__class__(self.name, self.theme)
        for attr in self.faceAttrs:
            copy.__setPrivAttr(attr, self.__getPrivAttr(attr))
        return copy
    ###########################################################################
    def __eq__(self, peer):
        if not isinstance(peer, Face):
            return NotImplemented
        getSelf, getPeer = self.__getPrivAttr, peer.__getPrivAttr
        return (self.name == peer.name and
                self.faceAttrs == peer.faceAttrs and
                all(getSelf(a) == getPeer(a) for a in self.faceAttrs))
    ###########################################################################
    def __neq__(self, peer):
        return not(self == peer)
    ###########################################################################
    def reset(self, peer):
        for attr in self.faceAttrs:
            self.__setPrivAttr(attr, peer.__getPrivAttr(attr))
    ###########################################################################
    def isSet(self, attr):
        return self.__getPrivAttr(attr) is not None
    ###########################################################################
    def getPeer(self, peerName):
        if peerName in (None, 'default'):
            return self.theme.defaultFace
        return self.theme.facesAttrs[peerName]
    ###########################################################################
    def inheritOrder(self):
        return (self.name, ) + self.getPeer(self.inherit).inheritOrder()
    ###########################################################################
    def derivesFrom(self, peer):
        return self.theme == peer.theme and peer.name in self.inheritOrder()
    ###########################################################################
    def getSource(self, attr):
        return self if self.isSet(attr) else \\
               self.getPeer(self.inherit).getSource(attr)
    ###########################################################################
    def getAttrHook_i(self, attr, val):
        return val if val is not None \\
               else getattr(self.getPeer(self.inherit), attr)
    ###########################################################################
    def setInheritHook_i(self, val):
        if val in (None, 'default'):
            return None
        if isinstance(val, str):
            theme = self.theme
            if not(hasattr(theme, 'faceAttrs')) or val in theme.faceAttrs:
                return val
        raise ValueError(\"Invalid inherit value=%s\" % val)
    ###########################################################################
    def getLispValue(self, attr, mapBool=('nil','t')):
        val = self.__getPrivAttr(attr)
        if val is None:
            return \"'unspecified\"
        if isinstance(val, bool):
            return mapBool[int(val)]
        if isinstance(val, str):
            return \"'%s\" % val
        if isinstance(val, Color):
            return '\"%s\"' % (val,)
        raise ValueError(\"getLispValue(%s): Invalid attr val=%s\" % (attr,val))
    ###########################################################################
    def setFaceSexpr(self, parms=\"\"):
        isSet, lispVal = self.isSet, self.getLispValue
        # add inhert attr
        if isSet('inherit'):
            parms += ' :inherit %s' % lispVal('inherit')
        # add foreground, background color attrs if attr is set
        for attr in filter(isSet, self.colorAttrs):
            parms += ' :%s %s' % (attr, lispVal(attr))
        # add bold attr
        if isSet('bold'):
            parms += ' :weight %s' % lispVal('bold', (\"'normal\", \"'bold\"))
        # add italic attr
        if isSet('italic'):
            parms += ' :slant %s' % lispVal('italic', (\"'normal\", \"'italic\"))
        # add underline attr
        if isSet('underline'):
            parms += ' :underline %s' % lispVal('underline')
        # add raised attr
        if isSet('raised'):
            val = lispVal('raised')
            if val == 't':
                val = '(:line-width 2 :color \"%s\" :style released-button)' % \\
                      self.foreground.blend(self.background)
            parms += ' :box %s' % val
        # make func call to set face in emacs
        return \"(themes-set-face '%s %s)\" % (self.name, parms)
###############################################################################
class DefaultFace(Face, metaclass=cldef.metaClass):
    __cldef = cldef.metaClass.wrapclass()
    inherit, underline, raised = __cldef.constants(None, False, False)
    ###########################################################################
    def __init__(self, theme):
        self.__super.__init__(\"default\", theme)
    ###########################################################################
    bold = property(lambda self: self.theme.font.weight == 'bold')
    italic = property(lambda self: self.theme.font.slant == 'italic')
    foreground = property(lambda self: self.theme.foregroundColor)
    background = property(lambda self: self.theme.backgroundColor)
    ###########################################################################
    def setFaceSexpr(self):
        return '(themes-set-defaults \"%s\" \"%s\" \"%s\")' % \\
               (self.foreground, self.background, self.theme.font)
    ###########################################################################
    def isSet(self, attr):
        return True
    ###########################################################################
    def inheritOrder(self):
        return (self.name,)
###############################################################################
class FacesAttrs(object, metaclass=cldef.metaClass):
    __cldef = cldef.metaClass.wrapclass()
    __cldef.slots(\"__faceMap __faces\")
    __cldef.field(\"theme\")
    ###########################################################################
    def __init__(self, theme, facesAttrs=None):
        self.theme,  self.__faceMap, self.__faces = theme, {}, []
        if facesAttrs is None:
            facesAttrs = self.loadFacesFromEmacs_i()
        for faceAttrs in facesAttrs:
            face = Face(theme=theme, \*\*faceAttrs)
            if self.__faceMap.setdefault(face.name, face) is not face:
                raise KeyError(\"Face name %r is not unique\" % face.name)
            self.__faces.append(face)
        for face in self.__faces:
            if face.inherit is not None:
                assert face.inherit in self.__faceMap
    ###########################################################################
    def copy(self):
        copy = self.__class__.__new__(self.__class__)
        copy.theme = self.theme
        copy.__faces = [f.copy() for f in self.__faces]
        copy.__faceMap = dict((f.name,f) for f in copy.__faces)
        return copy
    ###########################################################################
    def reset(self, peer):
        self.__faces = [f.copy() for f in peer.__faces]
        self.__faceMap = dict((f.name,f) for f in self.__faces)
    ###########################################################################
    def __len__(self):
        return len(self.__faceMap)
    ###########################################################################
    def __iter__(self):
        return iter(self.__faces)
    ###########################################################################
    def __eq__(self, peer):
        if not isinstance(peer, FacesAttrs):
            return NotImplemented
        return self.__faceMap == peer.__faceMap
    ###########################################################################
    def __neq__(self, peer):
        return not(self == peer)
    ###########################################################################
    def get(self, name, default=None):
        return self.__faceMap.get(name,default)
    ###########################################################################
    def __getitem__(self, name):
        return self.__faceMap[name]
    ###########################################################################
    def __contains__(self, name):
        return name in self.__faceMap
    ###########################################################################
    def loadFacesFromEmacs_i(self):
        facesSexpr = textwrap.dedent(\"\"\"\\
        (progn (add-to-list 'load-path \"%(elDir)s\")
               (add-to-list 'load-path \"%(elDir)s/python\")
               (add-to-list 'load-path \"%(elDir)s/imported\")
               (require 'themes)
               (write-region (themes-python-faces-attrs %(thColors)s)
                             nil \"%(outFile)s\")
               (kill-emacs))\"\"\")
        facesSexpr = ' '.join(facesSexpr.split())
        thColors = self.theme.foregroundColor, self.theme.backgroundColor
        outFile = (util.dirPath('\$TEMP')/'themeFaces.py').uniquify()
        facesSexpr %= {'elDir': util.Path('~/emacs').emacsName,
                       'thColors': '\"%s\" \"%s\"' % thColors,
                       'outFile': outFile.emacsName}
        try:
            subprocess.check_call(['emacs.exe', '--no-init-file',
                                   '--iconic', '--eval', facesSexpr],
                                   shell=True)
            return eval(outFile.text().replace('\\r\\n', '\\n'))
        finally:
            if outFile.isfile():
                outFile.remove()
###############################################################################
class ThemeChoice(object, metaclass=cldef.metaClass):
    __cldef = cldef.metaClass.wrapclass()
    __cldef.slots(\"__args\")
    ##########################################################################
    def __namedTests():
        _darkThreshold = 3 \* 255 \* 0.6
        def isThemeBackgroundDark(theme):
            return sum(theme.colorAttrs.backgroundColor) < _darkThreshold
        def isThemeBold(theme):
            return theme.fontAttrs.weight == 'bold'
        return dict(isThemeBackgroundDark = isThemeBackgroundDark,
                    isThemeFontBold = isThemeBold)
    __namedTests = __cldef.constant(__namedTests())
    ###########################################################################
    def __init__(self, theme, themeTest, onTrueValue, onFalseValue):
        self.__args= (theme, themeTest, onTrueValue, onFalseValue)
    ###########################################################################
    def __call__(self):
        theme, themeTest, onTrueValue, onFalseValue = self.__args
        if isinstance(themeTest, str):
            themeTest = self.__namedTests[themeTest]
        if themeTest(theme):
            return onTrueValue
        return onFalseValue
###############################################################################
class ThemeAccessor(object, metaclass=cldef.metaClass):
    __cldef = cldef.metaClass.wrapclass()
    __cldef.slots(\"__args\")
    ###########################################################################
    def __init__(self, theme, path):
        self.__args = (theme, path.split(\".\"))
    ###########################################################################
    def __call__(self):
        obj, path = self.__args
        for attr in path:
            obj = getattr(obj, attr)
        return obj
###############################################################################
class EmacsFrameTheme(object, metaclass=cldef.metaClass):
    __cldef = cldef.metaClass.wrapclass()
    __cldef.fields(\"name colorAttrs fontAttrs defaultFace facesAttrs \"
                   \"canDelete\")
    foregroundColor = property(lambda self: self.colorAttrs.foregroundColor)
    backgroundColor = property(lambda self: self.colorAttrs.backgroundColor)
    font = property(lambda self: self.fontAttrs.font)
    ###########################################################################
    def __str__(self):
        return util.nomen(self, \"%s:%s\" % (self.name, hex(id(self))))
    ###########################################################################
    def __init__(self, name, \*\*themeOpts):
        self.name = name
        self.colorAttrs = ColorAttrs()
        self.fontAttrs = FontAttrs()
        self.defaultFace = DefaultFace(self)
        facesAttrs = themeOpts.pop('facesAttrs', None)
        self.facesAttrs = FacesAttrs(self, facesAttrs=facesAttrs)
        self.canDelete = themeOpts.pop('canDelete', True)
        self.update(themeOpts)
    ###########################################################################
    def update(self, \*args, \*\*opts):
        if len(args) > 1:
            raise TypeError(\"Can be at most one positional argument\")
        opts = dict((args[0] if args else ()), \*\*opts)
        for attrs in (self.colorAttrs, self.fontAttrs):
            for attrName in attrs.attrNames:
                val = opts.pop(attrName, None)
                if val is not None:
                    setattr(attrs, attrName, val)
        _emptyParametersCheck(\*\*opts)
    ###########################################################################
    @classmethod
    def loadTheme(cls, themeFile, canDelete=True):
        themeFile = util.Path(themeFile, lambda f: f.isfile() and f.ext=='.el')
        themeSexpr = textwrap.dedent(\"\"\"\\
        (progn (add-to-list 'load-path \"%(elDir)s\")
               (add-to-list 'load-path \"%(elDir)s/python\")
               (add-to-list 'load-path \"%(elDir)s/imported\")
               (require 'themes)
               (load \"%(themeFile)s\")
               (write-region (themes-python-theme-attrs) nil \"%(outFile)s\")
               (kill-emacs))\"\"\")
        themeSexpr = ' '.join(themeSexpr.split())
        outFile = (util.dirPath('\$TEMP')/'themeAttrs.py').uniquify()
        themeSexpr %= {'elDir': util.Path('~/emacs').emacsName,
                       'themeFile': themeFile.stripext().emacsName,
                       'outFile': outFile.emacsName}
        try:
            subprocess.check_call(['emacs.exe', '--no-init-file',
                                   '--iconic', '--eval', themeSexpr],
                                   shell=True)
            themeOpts, facesAttrs = eval(outFile.text().replace('\\r\\n','\\n'))
            # convert the font string spec returned by emacs to fontAttrs
            # argumets expected by __init__
            font = Font(themeOpts.pop('font'))
            themeOpts.update(fontSize=font.size, fontWeight=font.weight,
                             fontSlant=font.slant, canDelete=canDelete)
            themeName = themeFile.namebase
            return cls(themeName, facesAttrs=facesAttrs, \*\*themeOpts)
        finally:
            if outFile.isfile():
                outFile.remove()
    ###########################################################################
    def copy(self, newThemeName, \*\*newThemeOpts):
        newTheme = copy.deepcopy(self)
        newTheme.__name = newThemeName
        newTheme.update(newThemeOpts)
        return newTheme
    ###########################################################################
    def __eq__(self, peer):
        if not isinstance(peer, EmacsFrameTheme):
            return NotImplemented
        return (self.name == peer.name and
                self.foregroundColor == peer.foregroundColor and
                self.backgroundColor == peer.backgroundColor and
                self.font == peer.font and
                self.facesAttrs == peer.facesAttrs)
    ###########################################################################
    def __neq__(self, peer):
        return not(self == peer)
    ###########################################################################
    @property
    def sexpr(self):
        cursor = Face('cursor', self, background=self.colorAttrs.cursorColor)
        faces = [self.defaultFace, cursor] + list(self.facesAttrs)
        return \"(progn\\n  %s)\" % \"\\n  \".join(f.setFaceSexpr() for f in faces)
    ###########################################################################
    __applyFormat = \"%(sexpr)s\\n(themes-save-cache-file (quote %(sexpr)s))\"
    ###########################################################################
    def applyTheme(self):
        lisp(self.__applyFormat % dict(sexpr=self.sexpr))
    ###########################################################################
    def accessor(self, \*p, \*\*kw):
        return ThemeAccessor(self, \*p, \*\*kw)
    ###########################################################################
    def choice(self, \*p, \*\*kw):
        return ThemeChoice(self, \*p, \*\*kw)
###############################################################################
def cacheFilePath(path):
    return util.Path(path, lambda f: not(f.isdir()))
###############################################################################
class EmacsFrameThemes(object, metaclass=cldef.metaClass):
    __cldef = cldef.metaClass.wrapclass()
    __cldef.slots(\"__themes\")
    __cldef.field('cacheFile', 'ri', sethook=cacheFilePath)
    __cldef.field(\"saveEnabled\", 'rw', initval=True, sethook=bool)
    __cldef.field(\"current\", 'rw',
                  fset=lambda obj,theme: obj.setCurrentTheme(theme))
    ###########################################################################
    def __new__(cls, themeMgr=None):
        if themeMgr is None:
            # pickle internals called __new__ SURPRISE!
            return object.__new__(cls)
        if themeMgr.cacheFile.isfile():
            # load themes from cache file
            return cls.loadCache_i(themeMgr.cacheFile)
        themes = object.__new__(cls)
        elispFiles = themeMgr.archive.files('\*.el')
        if elispFiles:
            # load themes from a collection (archive) of elisp files created
            # this module and emacs can use to initialize themes
            themes.loadArchive_i(elispFiles, themeMgr.cacheFile)
        else:
            # create themes using a few 'canned' themes
            themes.bootstrap_i(themeMgr.cacheFile)
        return themes
    ###########################################################################
    @classmethod
    def loadCache_i(cls, cacheFile):
        try:
            with cacheFile.open('rb') as cacheFP:
                themes = pickle.load(cacheFP)
        except:
            # If file has windows newlines '\\r\\n', read it as text which
            # converting newlines, encode to bytes and retry loading
            themes = pickle.loads(cacheFile.text().encode())
        assert isinstance(themes, cls)
        themes.__cacheFile = cacheFilePath(cacheFile)
        return themes
    ###########################################################################
    def loadArchive_i(self, elispFiles, cacheFile):
        self.__themes = []
        self.cacheFile, self.saveEnabled = cacheFile, False
        elispFiles = dict((f.namebase, f) for f in elispFiles)
        # load archive files in (standard themes) order. if no arcive file
        # is named after a standard theme, create it using provided args
        for thName,thOpts in ((d['name'],d) for d in self.standardThemes()):
            elispFile = elispFiles.pop(thName, None)
            if elispFile is not None:
                self.addTheme(EmacsFrameTheme.loadTheme(
                    elispFile, thOpts['canDelete']))
            else:
                self.addTheme(EmacsFrameTheme(\*\*thOpts))
        # load any remaining archive files
        for elispFile in sorted(elispFiles.values()):
            self.addTheme(EmacsFrameTheme.loadTheme(elispFile))
        self.current = self.__themes[0]
        self.saveEnabled = True
        self.save()
    ###########################################################################
    @staticmethod
    def standardThemes():
        return [dict(name='dark_frames', canDelete=False),
                dict(name='red_frames', canDelete=False,
                     backgroundColor=\"RGB:55/0/0\"),
                dict(name=\"green_frames\", canDelete=False,
                     backgroundColor=\"RGB:16/25/25\"),
                dict(name=\"blue_frames\", canDelete=False,
                     backgroundColor=\"RGB:0B/0B/2D\"),
                dict(name=\"light_frames\", canDelete=False,
                     foregroundColor='black',
                     backgroundColor='white')]
    ###########################################################################
    def bootstrap_i(self, cacheFile):
        self.__themes = []
        self.cacheFile, self.saveEnabled = cacheFile, False
        # create dark theme as current theme
        self.current = EmacsFrameTheme(\"dark_frames\", canDelete=False)
        # create red theme
        self.addTheme(EmacsFrameTheme(\"red_frames\", canDelete=False,
                                      backgroundColor=\"RGB:55/0/0\"))
        # create green theme
        self.addTheme(EmacsFrameTheme(\"green_frames\", canDelete=False,
                                      backgroundColor=\"RGB:16/25/25\"))
        # create blue theme
        self.addTheme(EmacsFrameTheme(\"blue_frames\", canDelete=False,
                                      backgroundColor=\"RGB:0B/0B/2D\"))
        # create light theme from default
        self.addTheme(EmacsFrameTheme(\"light_frames\", canDelete=False,
                                      foregroundColor='black',
                                      backgroundColor='white'))
        self.saveEnabled = True
        self.save()
    ###########################################################################
    def reset(self, prototype, copy=True):
        assert type(self) is type(prototype)
        if copy:
            prototype = copy.deepcopy(prototype)
        for attr in self.__slots__:
            setattr(self, attr, getattr(prototype, attr))
        self.save()
    ###########################################################################
    def save(self):
        if self.saveEnabled:
            with self.cacheFile.open('wb') as cacheFile:
                pickle.dump(self, cacheFile)
    ###########################################################################
    def addTheme(self, theme):
        if theme.name in self:
            raise KeyError(\"theme.name(%s) is not unique\" % theme.name)
        self.__themes.append(theme)
        self.save()
    ###########################################################################
    def getTheme(self, name, exact=True, default=None):
        if exact:
            for theme in self.__themes:
                if theme.name == name:
                    return theme
        else:
            name = name.strip().lower()
            for theme in self.__themes:
                if name in theme.name.lower():
                    return theme
        return default
    ###########################################################################
    def setCurrentTheme(self, theme):
        if isinstance(theme,str):
            theme = self.getTheme(theme)
        if isinstance(theme, EmacsFrameTheme):
            self.__current = theme
            if theme not in self:
                self.addTheme(theme)
        else:
            raise ValueError(\"%s invalid theme\" % theme)
    ###########################################################################
    def removeTheme(self, theme, force=False):
        # arg can be either a theme instance or a str naming the theme
        if isinstance(theme,str):
            theme = self.getTheme(theme)
        if not force:
            if len(self.__themes) == 1:
                raise ValueError(\"Can't remove last theme\")
            if not theme.canDelete:
                raise ValueError(\"Theme %s can't be removed\" % theme.name)
        # remove the theme from both the themes list
        self.__themes.remove(theme)
        # adjust current attr if theme deleted was current one
        if theme == self.__current:
            try:
                self.__current = self.__themes[0]
            except IndexError:
                del self.__current
        self.save()
    ###########################################################################
    def swapThemes(self, theme1, theme2):
        if isinstance(theme1, str):
            theme1 = self.getTheme(theme1)
        if isinstance(theme2, str):
            theme2 = self.getTheme(theme2)
        index1 = self.__themes.index(theme1)
        index2 = self.__themes.index(theme2)
        self.__themes[index2] = theme1
        self.__themes[index1] = theme2
    ###########################################################################
    def __iter__(self):
        return iter(self.__themes)
    ###########################################################################
    def __len__(self):
        return len(self.__themes)
    ###########################################################################
    def __contains__(self, item):
        if isinstance(item, str):
            return item in (theme.name for theme in self.__themes)
        return item in self.__themes
###############################################################################
class EmacsFrameThemeManager(datatypes.Singleton, metaclass=cldef.metaClass):
    __cldef = cldef.metaClass.wrapclass()
    __cldef.fields('themesDir cacheFile archive')
    __cldef.field('themes', 'r', initfunc=lambda obj: EmacsFrameThemes(obj))
    currentTheme = property(lambda self: self.themes.current)
    ###########################################################################
    def newSingleton_i(self, themesDir=None):
        self.__super.newSingleton_i()
        if themesDir is None:
            themesDir = '\$APPDATA/_emacsthemes_v%d' % version
        self.themesDir = util.dirPath(themesDir)
        archive = self.themesDir/'archive'
        if not archive.exists():
            archive.mkdir()
        self.archive = util.dirPath(archive)
        self.cacheFile = self.themesDir/'cache.pkl'
    ###########################################################################
    def applyNamedTheme(self, themeName, exact=True):
        themeName = themeName.replace(\"-\",\"_\")
        self.__themes.current = self.__themes.getTheme(themeName, exact)
        self.currentTheme.applyTheme()
    ###########################################################################
    def setFrameTheme(self, themeName, \*\*themeOpts):
        self.__themes.current = themeName
        self.currentTheme.update(themeOpts)
        self.currentTheme.applyTheme()
    ###########################################################################
    def gui(self):
        global EmacsFrameThemesApp
        try:
            guiRun = EmacsFrameThemesApp.run
        except NameError:
            from themes.thgui import EmacsFrameThemesApp
            guiRun = EmacsFrameThemesApp.run
        guiRun(self, tuple(i+50 for i in emacsPosition()))
    ###########################################################################
    def archiveThemes(self):
        textFormat = textwrap.dedent(\"\"\"\\
        ;;; %s :: %s -\*-Emacs-Lisp-\*-
        ;;; -- Used by themes.el for persistance of current frame theme
        ;;;    settings across emacs invocations
        %s
        \"\"\")
        for theme in self.themes:
            archiveFile = self.archive/(theme.name + '.el')
            timeStr = datetime.datetime.now().strftime(\"%a %b %d %H:%M:%S %Y\")
            archiveFile.write_text(
                textFormat % (archiveFile.name, timeStr, theme.sexpr))
"
   'python-mode
   'py-verbose-p
   (goto-char 25548)
   (py-forward-def-or-class)
   (should (looking-back "return themes" (line-beginning-position)))))

(ert-deftest py-execute-region-ipython-test-1gyFLs ()
  (py-test
   "#! /usr/bin/env python3
print(u'\\xA9')"
   'python-mode
   'py-verbose-p
   (goto-char (point-max))
   (push-mark)
   (beginning-of-line)
   (if (not (executable-find "ipython"))
       (message "py-execute-region-ipython-test-1gyFLs: %s" "No executable found")
     (py-execute-region-ipython (region-beginning) (region-end))
     (set-buffer "*IPython*")
     (string-match "@" (buffer-substring-no-properties (point-min) (point-max))))))

(ert-deftest py-ert-respect-paragraph-1294829-test-dpmi5s ()
  (py-test-point-min
      "# py-fill-paragraph doesn\';t respect existing paragraph breaks when
# reflowing the docstring, e.g.

def foo(self)
    \"\"\"First one-line summary.

    Some other stuff which I don\'t want a paragraph break inserted into
    the middle of.

    And another para hjkdfgh fdjkg hfdjkg hdfjk ghdfk ghjkdf
    ghjkdf ghjdf ghjdkf k
    \"\"\"

def foo(self)
    \"\"\"Second one-line summary. Some other stuff which I don\'t want a
paragraph

    break inserted into the middle of. And another para hjkdfgh
fdjkg
    hfdjkg hdfjk ghdfk ghjkdf ghjkdf ghjdf ghjdkf k \"\"\"

# I feel it would be better if it didn\'t attempt to
# reflow the whole docstring, rather just reflow the
# particular paragraph within it which the point is
# positioned in.

# It would also be good if it could avoid mangling parameter
# descriptions like this:

def foo(self):
    \"\"\"Summary line.

    Foo bar fhgdjkfd hgjfd hgjkfd ghjkdf ghjkdf hgjdf ghjkdf
hgjdf hjgk dfhjkg dfhjkg dfhjkg fdhjkg hjfdkg

    Parameters
    ----------
    endog : array-like
        1-d endogenous response variable. The dependent variable.
    exog : array-like
        A nobs x k array where `nobs` is the number of
observations and `k`
        is the number of regressors. An interecept is not
included by default
        and should be added by the user. See
        `statsmodels.tools.add_constant`.\"\"\"

def foo(self):
    \"\"\"Summary line. Foo bar fhgdjkfdhgjfd hgjkfd ghjkdf ghjkdf
hgjdf

    ghjkdf hgjdf hjgk dfhjkg dfhjkg dfhjkg fdhjkghjfdkg
Parameters
    ---------- endog : array-like 1-d endogenous response
variable. The
    dependent variable. exog : array-like A nobs x karray where
`nobs`
    is the number of observations and `k` is the number of
regressors.
    An interecept is not included by default and should be added
by the
    user. See `statsmodels.tools.add_constant`.
    \"\"\"

# Failing that though, if I can at least choose to
# reflow individual paragraphs in the docstring and
# leave others intact, I can format these things
# manually while still being able to flow other
# paragraphs using M-q.
"
    'python-mode
    py-debug-p
    (goto-char (point-min))
    ;; (font-lock-fontify-region (point-min)(point-max))
    (search-forward "Some other" nil t 1)
    (sit-for 0.1 t)
    (fill-paragraph)
    (forward-line -2)
    (should (not (py-empty-line-p)))
    (forward-line 1)
    (should (eq (char-after) ?\n))))

(ert-deftest py-ert-respect-paragraph-1294829-test-s7lFth ()
  (py-test-point-min
      "# py-fill-paragraph doesn\';t respect existing paragraph breaks when
# reflowing the docstring, e.g.

def foo(self)
    \"\"\"First one-line summary.

    Some other stuff which I don\'t want a paragraph break inserted into
    the middle of.

    And another para hjkdfgh fdjkg hfdjkg hdfjk ghdfk ghjkdf
    ghjkdf ghjdf ghjdkf k
    \"\"\"

def foo(self)
    \"\"\"Second one-line summary. Some other stuff which I don\'t want a
paragraph

    break inserted into the middle of. And another para hjkdfgh
fdjkg
    hfdjkg hdfjk ghdfk ghjkdf ghjkdf ghjdf ghjdkf k \"\"\"

# I feel it would be better if it didn\'t attempt to
# reflow the whole docstring, rather just reflow the
# particular paragraph within it which the point is
# positioned in.

# It would also be good if it could avoid mangling parameter
# descriptions like this:

def foo(self):
    \"\"\"Summary line.

    Foo bar fhgdjkfd hgjfd hgjkfd ghjkdf ghjkdf hgjdf ghjkdf
hgjdf hjgk dfhjkg dfhjkg dfhjkg fdhjkg hjfdkg

    Parameters
    ----------
    endog : array-like
        1-d endogenous response variable. The dependent variable.
    exog : array-like
        A nobs x k array where `nobs` is the number of
observations and `k`
        is the number of regressors. An interecept is not
included by default
        and should be added by the user. See
        `statsmodels.tools.add_constant`.\"\"\"

def foo(self):
    \"\"\"Summary line. Foo bar fhgdjkfdhgjfd hgjkfd ghjkdf ghjkdf
hgjdf

    ghjkdf hgjdf hjgk dfhjkg dfhjkg dfhjkg fdhjkghjfdkg
Parameters
    ---------- endog : array-like 1-d endogenous response
variable. The
    dependent variable. exog : array-like A nobs x karray where
`nobs`
    is the number of observations and `k` is the number of
regressors.
    An interecept is not included by default and should be added
by the
    user. See `statsmodels.tools.add_constant`.
    \"\"\"

# Failing that though, if I can at least choose to
# reflow individual paragraphs in the docstring and
# leave others intact, I can format these things
# manually while still being able to flow other
# paragraphs using M-q.
"
    'python-mode
    py-debug-p
    (goto-char (point-min))
    (search-forward "one-line summary." nil t 1)
    (when py-debug-p (message "fill-column: %s" fill-column))
    (fill-paragraph)
    (forward-line 1)
    (sit-for 0.1 t)
    (should (py-empty-line-p))
    (search-forward "Foo bar" nil t 1)
    (fill-paragraph)
    (forward-line 2)
    (should (eq (char-after) ?\n))))

(ert-deftest py-indent-bug63959-test-6ZlhPF ()
  (py-test
   "def f():
    \"\"\"
    Return nothing.
    .. NOTE::
        First note line
    second note line\"\"\"
    pass
"
   'python-mode
   'py-verbose-p
   (goto-char (point-max))
   (search-backward "First")
   (sit-for 0.1)
   (should (eq 4 (py-compute-indentation)))))


(ert-deftest py-indent-bug63959-test-Bfr7rA ()
  (py-test
   "def f():
    \"\"\"
    Return nothing.
    .. NOTE::
        First note line
    second note line\"\"\"
    pass
"
   'python-mode
   'py-verbose-p
   (goto-char (point-max))
   (search-backward "Return")
   (should (eq 4 (py-compute-indentation)))))

(ert-deftest py-indent-or-complete-7NWa5T ()
  (py-test
   "def foo:
    pass\n\npri"
   'python-mode
   'py-verbose-p
   (goto-char (point-max))
   (skip-chars-backward " \t\r\n\f")
   (py-indent-or-complete)
   (should (looking-back "print.?" (line-beginning-position)))))

(ert-deftest py-ert-moves-up-fill-paragraph-pep-257-nn-BBJoDt ()
  (let ((py-docstring-style 'pep-257-nn))
    (py-test-point-min
     "# r1416
def baz():
    \"\"\"Hello there. This is a multiline function definition. Don= 't worry, be happy. Be very very happy. Very. happy. This is a multiline function definition. Don= 't worry, be happy. Be very very happy. Very. happy. This is a multiline function definition. Don= 't worry, be happy. Be very very happy. Very. Not unhappy.
    Now this is a multiline function definition. Don= 't worry, be happy. Be very very happy. Very glad.
    \"\"\"
    return 7
"
     'python-mode
     'py-verbose-p
     (search-forward "\"\"\"")
     (fill-paragraph)
     (goto-char (point-min))
     (search-forward "unhappy")
     (forward-line 1)
     (should (py-empty-line-p))
     (search-forward "Now")
     (should (eq 4 (current-indentation)))
     (search-forward "glad")
     (forward-line 1)
     ;; (sit-for 1)
     (back-to-indentation)
     ;; (sit-for 1)
     ;; (should (eq (char-after) 34))
     )))

(ert-deftest py-ert-moves-up-fill-paragraph-django-BVA4Jt ()
  (let ((py-docstring-style 'django))
    (py-test-point-min
     "# r1416
def baz():
    \"\"\"Hello there. This is a multiline function definition. Don't wor ry, be happy. Be very very happy. Very. happy. This is a multiline function definition. Don't worry, be happy. Be very very happy. Very. happy. This is a multiline function definition. Don't worry, be happy. Be very very happy. Very. happy.
    This is a multiline function definition. Don't worry, be happy. Be very very happy. Very. happy.
    \"\"\"
    return 7
"
     'python-mode
     'py-verbose-p
     ;; (font-lock-ensure)
     (goto-char 49)
     (fill-paragraph)
     (search-forward "multiline" nil t 3)
     (forward-line 2)
     (should (py-empty-line-p)))))

(ert-deftest py-fast-send-string-no-output-VxbcvH ()
  (py-test
   "print(234)"
   'python-mode
   'py-verbose-p
   (py--fast-send-string-no-output (buffer-substring-no-properties (point-min) (point-max)))
   (set-buffer "*Python3 Fast*")
   ;; (when py-debug-p (switch-to-buffer (current-buffer)))
   (when py-debug-p (switch-to-buffer "*Python3 Fast*"))
   (should (eq 1 (point-max)))))

(ert-deftest py-send-string-no-output-VxbcvH ()
  (py-test
   "print(234)"
   'python-mode
   'py-verbose-p
   (py-send-string-no-output (buffer-substring-no-properties (point-min) (point-max)))
   (set-buffer "*Python3*")
   (goto-char (point-max))
   ;; (when py-debug-p (switch-to-buffer (current-buffer)))
   (when py-debug-p (switch-to-buffer "*Python3*"))
   (should-not (looking-back "123" (line-beginning-position)))))

(ert-deftest py-pdbtrack-test-H6CpKY ()
  (py-test
   "import pdb
import sys
import os
pdb.set_trace()
args = sys.argv
def usage():
    print(\"\"\"Fehler: %s
Es muß die aufzurufende Ziehungszahl als Argument angegeben werden:
'python roulette.py 1, 'python roulette.py 2', ... 'python roulette.py n'.
\"\"\" % (
          os.path.basename(sys.argv\[0])))
def main():
    if len(sys.argv) == 1:
        usage()
        # sys.exit()
"
   'python-mode
   'py-verbose-p
   (save-excursion
     (let ((inhibit-field-text-motion t)
	   py-split-window-on-execute
	   py-switch-buffers-on-execute-p)
       (py-execute-buffer)
       (should (buffer-live-p (get-buffer "*Python3*")))
       (set-buffer (get-buffer "*Python3*"))
       (goto-char (point-max))
       (should (string-match "Pdb" (buffer-substring-no-properties (line-beginning-position) (point-max))))))))

(ert-deftest highlight-typed-variables-in-python-41684-test-ZFhHGT ()
  (py-test
   ;; https://lists.gnu.org/archive/html/bug-gnu-emacs/2020-06/msg00128.html
   "foo: int = 1"
   'python-mode
   'py-verbose-p
   (font-lock-ensure)
   (goto-char (point-max))
   (search-backward "foo")
   (let ((erg (face-at-point)))
     (sit-for 0.1)
     (should (equal erg 'py-variable-name-face)))))

(provide 'py-extra-tests)
;;; py-extra-tests.el ends here

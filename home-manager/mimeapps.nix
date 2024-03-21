{ ... }: {
  xdg.mimeApps = {
    enable = true;

    defaultApplications = let
      editor = "emacsclient.desktop";
      browser = "firefox.desktop";
      mediaPlayer = "vlc.desktop";
      fileManager = "org.kde.dolphin.desktop";
      emailClient = "userapp-Thunderbird-Y9ZY61.desktop";
      torrentApp = "userapp-transmission-gtk-06T351.desktop";
      imageViewer = "org.xfce.ristretto.desktop";
      pdfReader = "okularApplication_pdf.desktop";
      richTextEditor = "writer.desktop";
      presentation = "impress.desktop";
      spreadsheet = "calc.desktop";
      audioPlayer = "vlc.desktop";

    in {
      "application/epub+zip" = [ editor ];
      "application/xml" = [ editor ];
      "text" = [ editor ];
      "text/css" = [ editor ];
      "text/html" = [ editor ];
      "text/htmlh" = [ editor ];
      "text/javascript" = [ editor ];
      "text/julia" = [ editor ];
      "text/markdown" = [ editor ];
      "text/org" = [ editor ];
      "text/plain" = [ editor ];
      "text/rust" = [ editor ];
      "text/sgml" = [ editor ];
      "text/tcl" = [ editor ];
      "text/troff" = [ editor ];
      "text/x-bibtex" = [ editor ];
      "text/x-c++hdr" = [ editor ];
      "text/x-c++src" = [ editor ];
      "text/x-cmake" = [ editor ];
      "text/x-cobol" = [ editor ];
      "text/x-common-lisp" = [ editor ];
      "text/x-crystal" = [ editor ];
      "text/x-csharp" = [ editor ];
      "text/x-csrc" = [ editor ];
      "text/x-elixir" = [ editor ];
      "text/x-emacs-lisp" = [ editor ];
      "text/x-erlang" = [ editor ];
      "text/x-go" = [ editor ];
      "text/x-groovy" = [ editor ];
      "text/x-haskell" = [ editor ];
      "text/x-java" = [ editor ];
      "text/x-kotlin" = [ editor ];
      "text/x-log" = [ editor ];
      "text/x-lua" = [ editor ];
      "text/x-makefile" = [ editor ];
      "text/x-nim" = [ editor ];
      "text/x-ocaml" = [ editor ];
      "text/x-patch" = [ editor ];
      "text/x-python" = [ editor ];
      "text/x-python3" = [ editor ];
      "text/x-qml" = [ editor ];
      "text/x-readme" = [ editor ];
      "text/x-sass" = [ editor ];
      "text/x-scheme" = [ editor ];
      "text/x-scss" = [ editor ];
      "text/x-systemd-unit" = [ editor ];
      "text/x-tex" = [ editor ];
      "text/x-todo-txt" = [ editor ];

      "application/pdf" = [ pdfReader editor ];

      "application/x-extension-htm" = [ browser ];
      "application/x-extension-html" = [ browser ];
      "application/x-extension-shtml" = [ browser ];
      "application/x-extension-xht" = [ browser ];
      "application/x-extension-xhtml" = [ browser ];
      "application/xhtml+xml" = [ browser ];
      "x-scheme-handler/chrome" = [ browser ];
      "x-scheme-handler/http" = [ browser ];
      "x-scheme-handler/https" = [ browser ];

      "application/xspf+xml" = [ mediaPlayer ];
      "video/mp4" = [ mediaPlayer ];
      "video/ogg" = [ mediaPlayer ];
      "video/webm" = [ mediaPlayer ];
      "video/mjpeg" = [ mediaPlayer ];
      "video/x-ogm+ogg" = [ mediaPlayer ];
      "video/x-matroska" = [ mediaPlayer ];

      "audio/aac" = [ audioPlayer ];
      "audio/flac" = [ audioPlayer ];
      "audio/midi" = [ audioPlayer ];
      "audio/mp4" = [ audioPlayer ];
      "audio/mpeg" = [ audioPlayer ];
      "audio/ogg" = [ audioPlayer ];
      "audio/webm" = [ audioPlayer ];
      "audio/x-flac+ogg" = [ audioPlayer ];
      "audio/x-vorbis+ogg" = [ audioPlayer ];

      "inode/directory" = [ fileManager ];

      "message/rfc822" = [ emailClient ];
      "x-scheme-handler/mailto" = [ emailClient ];
      "x-scheme-handler/mid" = [ emailClient ];

      "x-scheme-handler/magnet" = [ torrentApp ];

      "image/webp" = [ "okularApplication_kimgio.desktop" ];
      "image" = [ imageViewer ];
      "image/jpeg=org.xfce.ristretto.desktop;" = [ imageViewer ];
      "image/png=org.xfce.ristretto.desktop;" = [ imageViewer ];
      "image/svg+xml=org.xfce.ristretto.desktop;" = [ imageViewer ];

      "application/vnd.oasis.opendocument.text" = [ richTextEditor ];
      "application/vnd.openxmlformats-officedocument.wordprocessingml.document" =
        [ richTextEditor ];
      "text/richtext" = [ richTextEditor ];
      "application/vnd.oasis.opendocument.presentation" = [ presentation ];
      "application/vnd.openxmlformats-officedocument.presentationml.presentation" =
        [ presentation ];
      "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet" =
        [ spreadsheet ];
      "text/csv" = [ spreadsheet ];
      "text/spreadsheet" = [ spreadsheet ];
    };
  };
}

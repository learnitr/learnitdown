#' Insert code in an R Markdown document to diagnose personal data information
#'
#' This function is used, inside an R Markdown document, to dynamically
#' elaborate a short report on the client-side (by using JavaScript code) that
#' displays the login status of the user (login, contextual data, ...).
#'
#' @param block Should the diagnostic be included in a block construct
#' (not if `NULL`)?
#' @param lang The language to use for diagnostic output (only `'en'` or `'fr'` currently)
#'
#' @return Nothing, the function is used for its side-effect of adding
#' diagnostic code in the document
#' @export
#'
#' @details
#' Wherever you want to display this diagnostic report in your bookdown page,
#' add on its own line the following instruction:
#' 'r learndown::diagnose_login()' between backquotes (you can also change
#' `lang`, if you want).
diagnose_login <- function(block = "info", lang = "en") {
  switch(lang,
    en = .diagnose_login_en(block = block),
    fr = .diagnose_login_fr(block = block),
    stop("This language is not supported yet"))
}

.diagnose_login_en <- function(block = "info") {
  if (is.null(block)) {
    start <- ""
    end <- ""
  } else {
    start <- paste0("
::: {.", block, "}")
    end <- ":::

"
  }

  paste0(start, "

::: {#diagnostic-en}

*Please wait, diagnostic in course...*

<script>
function clearStorageEn() {
  if (window.localStorage) {
    localStorage.clear();
  }
  var msg = 'Local personal data deleted!' + '\\nThe page will be reloaded...' + '\\n\\n(if this is an error, close the page and relaunch it from within Moodle).';
  if (window.self !== window.parent) {
    parent.alert(msg);
  } else {
    window.alert(msg);
  }
  window.location.href=window.location.href;
}

function diagnoseSystemEn() {
  var container = document.getElementById('diagnostic-en');
  if (container === null) {
    return;
  }
  var content = '';

  if (!window.localStorage) {
    content = content + \"<p>You are:</p>\\n<ul>\";
    content = content + \"\\n<li>Anonymous on this site (local storage unavailable).</li>\";
    content = content + \"\\n<li>The content <b>is not</b> contextualized for a particular course.</li>\";
    content = content + \"\\n<li>Results for various exercises <b>will not be recorded</b>.</li>\";
    content = content + \"\\n</ul>\";
    content = content + \"\\n<p>If you are <i>not</i> a student that takes a course linking with this pedagogical material, this is OK, you can continue your journey... <b>Welcome!</b></p>\";
    content = content + \"\\n<p>On the other hand, if your cursus requires you to be properly recorded here, then close this page and restart it from your institution elearning platform (Moodle, ...). If your data are still incorrect, contact your teachers.</p>\";

  } else {
    // Explore the content of the storage
    // Students are recognized by their login (equivalent to their GitHub login)
    // but we also show their iemail (institutional email, or Wordpress email)
    var login = localStorage.getItem('login');
    if (login === null) {
      login = '';
    }
    var email = localStorage.getItem('email');
    if (email === null) {
      email = '';
    }
    var iemail = localStorage.getItem('iemail');
    if (iemail === null) {
      iemail = '';
    }
    // Compare both emails, if they are different, display both of them
    if (email != iemail && iemail != '') {
      email = email + '</u> (institutional email: <u>' + iemail + ')';
    }

    // Construct an identification string
    content = content + \"<p><b>Content on these pages is contextual. </b>Please, check the following data are correct:</p>\\n<ul>\";

    var registered = false;
    if (login == '') {
      // Not registered
      content = content + \"\\n<li><u>You are anonymous on this site.</u> <b>Your progress in the exercises will not be recorded.</b></li>\";

    } else {
      // Registered user
      registered = true;
      if (login != '') {
        content = content + \"\\n<li>Login&nbsp;: <u>\" + login + \"</u></li>\";
      }
      if (email != '') {
        content = content + \"\\n<li>Email&nbsp;: <u>\" + email + \"</u></li>\";
      }
      content = content + \"\\n<b>Your progress in the exercises will be recorded under that identity.</b>\";
    }

    // institution and icourse possibly change the content. So, must be checked!
    var context = false;
    var institution = localStorage.getItem('institution');
    if (institution == null || institution == '') {
      institution = 'undefined';
    } else {
      context = true;
    }
    var icourse = localStorage.getItem('icourse');
    if (icourse == null || icourse == '') {
      icourse = 'undefined';
    } else {
      context = true;
      var ictitle = localStorage.getItem('ictitle');
      if (ictitle != null && ictitle != '') {
        icourse = ictitle + ' (' + icourse + ')';
      }
    }
    content = content + \"\\n<li>Course&nbsp;: <u>\" + icourse + \"</u></li>\";
    content = content + \"\\n<li>Institution&nbsp;: <u>\" + institution + \"</u></li>\";
    content = content + \"\\n</ul>\";
    if (context) {
      content = content + \"<p><b>Content will be adapted to this particular context. </b>Check that these data are correct, or close this page and reopen it from your institutional elearning platform (Moodle, ...). If your data are still incorrect, contact your teachers.</p>\";
    } else {// No context
      content = content + \"<p><b>Without a particular course context, you will see a general content. </b>If this is OK... Welcome! Otherwise close this page and restart it from your institutional elearning platform (Moodle, ...). If your data are still incorrect, contact your teachers.</p>\";
    }

    // If registered user and/or special context, offer to reset it
    if (registered || context) {
      content = content + \"\\n<p>If you prefer to visit this site anonymously and record no activity in the exercises, you can delete your personal informations by clicking on the button bellow.</p>\";
      content = content + '<button onclick=\"clearStorageEn()\">Delete my personal data</button>';
    }
    // Those variables are possibly defined, but they are not tested here (yet)
    //var displayname = localStorage.getItem('displayname');
    //var firstname = localStorage.getItem('firstname');
    //var lastname = localStorage.getItem('lastname');
    //var ifirstname = localStorage.getItem('ifirstname');
    //var ilastname = localStorage.getItem('ilastname');
    //var iid = localStorage.getItem('iid');
    //var iurl = localStorage.getItem('iurl');
    //var iref = localStorage.getItem('iref');
  }

  container.innerHTML = content;
}
diagnoseSystemEn();
</script>
<noscript><b>The system diagnostic cannot be run because Javascript is not enabled, but it is mandatory for thei site! All the interactive and contextual content will be unusable. Please, do activate Javascript...</b></noscript>

:::
", end)
}

.diagnose_login_fr <- function(block = "info") {
  if (is.null(block)) {
    start <- ""
    end <- ""
  } else {
    start <- paste0("
::: {.", block, "}")
    end <- ":::

"
  }

  paste0(start, "

::: {#diagnostic-fr}

*Patientez s'il-vous-plait, diagnostic en cours...*

<script>
function clearStorageFr() {
  if (window.localStorage) {
    localStorage.clear();
  }
  var msg = 'Informations personnelles locales effacees !' + '\\nLa page va etre rechargee...' + \"\\n\\n(s'il s'agit d'une fausse manoeuvre, fermer la page et relancez-la depuis Moodle).\";
  if (window.self !== window.parent) {
    parent.alert(msg);
  } else {
    window.alert(msg);
  }
  window.location.href=window.location.href;
}

function diagnoseSystemFr() {
  var container = document.getElementById('diagnostic-fr');
  if (container === null) {
    return;
  }
  var content = '';

  if (!window.localStorage) {
    content = content + \"<p>Vous &ecirc;tes&nbsp;:</p>\\n<ul>\";
    content = content + \"\\n<li>Anonyme sur ce site (stockage local indisponible).</li>\";
    content = content + \"\\n<li>Le contenu <b>n'est pas</b> contextualis&eacute; par rapport &agrave; un cours en particulier.</li>\";
    content = content + \"\\n<li>Le r&eacute;sultat de certains exercices <b>ne sera pas enregistr&eacute;</b>.</li>\";
    content = content + \"\\n</ul>\";
    content = content + \"\\n<p>Si vous n'&ecirc;tes <i>pas</i> un &eacute;tudiant qui suit un cours li&eacute; &agrave; ce mat&eacute;riel p&eacute;dagogique, c'est normal, vous pouvez continuer votre visite... <b>Bienvenue&nbsp;!</b></p>\";
    content = content + \"\\n<p>En revanche, si votre cours n&eacute;cessite que vous soyez d&ucirc;ment enregistr&eacute;, alors fermez cette page et relancer-l&agrave; depuis le syst&egrave;me d'apprentissage en ligne de votre Universit&eacute; (Moodle, ...). Si les donn&eacute;es sont toujours incorrectes, contactez vos enseignants.</p>\";

  } else {
    // Explore the content of the storage
    // Students are recognized by their login (equivalent to their GitHub login)
    // but we also show their iemail (institutional email, or Wordpress email)
    var login = localStorage.getItem('login');
    if (login === null) {
      login = '';
    }
    var email = localStorage.getItem('email');
    if (email === null) {
      email = '';
    }
    var iemail = localStorage.getItem('iemail');
    if (iemail === null) {
      iemail = '';
    }
    // Compare both emails, if they are different, display both of them
    if (email != iemail && iemail != '') {
      email = email + '</u> (email institutionnel : <u>' + iemail + ')';
    }

    // Construct an identification string
    content = content + \"<p><b>Le contenu de ce cours est contextuel.</b> V&eacute;rifiez les informations suivantes, s'il-vous-plait&nbsp;:</p>\\n<ul>\";

    var registered = false;
    if (login == '') {
      // Not registered
      content = content + \"\\n<li><u>Vous &ecirc;tes anonyme sur ce site.</u> <b>Votre progression dans les exercices ne sera pas enregistr&eacute;e.</b></li>\";

    } else {
      // Registered user
      registered = true;
      if (login != '') {
        content = content + \"\\n<li>Login&nbsp;: <u>\" + login + \"</u></li>\";
      }
      if (email != '') {
        content = content + \"\\n<li>Email&nbsp;: <u>\" + email + \"</u></li>\";
      }
      content = content + \"\\n<b>Votre progression dans les exercices sera enregistr&eacute;e sous cette identit&eacute;.</b>\";
    }

    // institution and icourse possibly change the content. So, must be checked!
    var context = false;
    var institution = localStorage.getItem('institution');
    if (institution == null || institution == '') {
      institution = 'ind&eacute;termin&eacute;e';
    } else {
      context = true;
    }
    var icourse = localStorage.getItem('icourse');
    if (icourse == null || icourse == '') {
      icourse = 'ind&eacute;termin&eacute;';
    } else {
      context = true;
      var ictitle = localStorage.getItem('ictitle');
      if (ictitle != null && ictitle != '') {
        icourse = ictitle + ' (' + icourse + ')';
      }
    }
    content = content + \"\\n<li>Cours&nbsp;: <u>\" + icourse + \"</u></li>\";
    content = content + \"\\n<li>Institution&nbsp;: <u>\" + institution + \"</u></li>\";
    content = content + \"\\n</ul>\";
    if (context) {
      content = content + \"<p><b>Le contenu sera adapt&eacute; en fonction de ce contexte.</b> V&eacute;rifiez qu'il est correct, sinon fermez cette page et relancer-l&agrave; depuis le syst&egrave;me d'apprentissage en ligne de votre Universit&eacute; (Moodle, ...). Si les donn&eacute;es sont toujours incorrectes, contactez vos enseignants.</p>\";
    } else {// No context
      content = content + \"<p><b>N'&eacute;tant dans aucun contexte de cours particulier, vous n'aurez acc&egrave;s qu'&agrave un contenu g&eacute;n&eacute;raliste.</b> Si c'est ce que vous souhaitez... Bienvenue&nbsp;! Sinon, fermez cette page et relancer-l&agrave; depuis le syst&egrave;me d'apprentissage en ligne de votre Universit&eacute; (Moodle, ...). Si les donn&eacute;es sont toujours incorrectes, contactez vos enseignants.</p>\";
    }

    // If registered user and/or special context, offer to reset it
    if (registered || context) {
      content = content + \"\\n<p>Pour explorer ces pages de mani&egrave;re anonyme et n'enregistrer aucune activit&eacute;, vous pouvez &eacute;liminez vos informations personnelles en cliquant sur le bouton juste ci-dessous.</p>\";
      content = content + '<button onclick=\"clearStorageFr()\">Effacer mes donn&eacute;es personnelles</button>';
    }
    // Those variables are possibly defined, but they are not tested here (yet)
    //var displayname = localStorage.getItem('displayname');
    //var firstname = localStorage.getItem('firstname');
    //var lastname = localStorage.getItem('lastname');
    //var ifirstname = localStorage.getItem('ifirstname');
    //var ilastname = localStorage.getItem('ilastname');
    //var iid = localStorage.getItem('iid');
    //var iurl = localStorage.getItem('iurl');
    //var iref = localStorage.getItem('iref');
  }

  container.innerHTML = content;
}
diagnoseSystemFr();
</script>
<noscript><b>Le diagnostic ne peut se terminer car Javascript ne fonctionne pas, or il est indispensable pour ce site ! Tout le contenu interactif et contextuel sera inutilisable. Veuillez activer Javascript, s'il-vous-plait...</b></noscript>

:::
", end)
}

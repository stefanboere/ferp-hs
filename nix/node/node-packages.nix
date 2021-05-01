# This file has been generated by node2nix 1.9.0. Do not edit!

{ nodeEnv, fetchurl, fetchgit, nix-gitignore, stdenv, lib
, globalBuildInputs ? [ ] }:

let
  sources = {
    "@rollup/pluginutils-3.1.0" = {
      name = "_at_rollup_slash_pluginutils";
      packageName = "@rollup/pluginutils";
      version = "3.1.0";
      src = fetchurl {
        url =
          "https://registry.npmjs.org/@rollup/pluginutils/-/pluginutils-3.1.0.tgz";
        sha512 =
          "GksZ6pr6TpIjHm8h9lSQ8pi8BE9VeubNT0OMJ3B5uZJ8pz73NPiqOtCog/x2/QzM1ENChPKxMDhiQuRHsqc+lg==";
      };
    };
    "@types/buble-0.19.2" = {
      name = "_at_types_slash_buble";
      packageName = "@types/buble";
      version = "0.19.2";
      src = fetchurl {
        url = "https://registry.npmjs.org/@types/buble/-/buble-0.19.2.tgz";
        sha512 =
          "uUD8zIfXMKThmFkahTXDGI3CthFH1kMg2dOm3KLi4GlC5cbARA64bEcUMbbWdWdE73eoc/iBB9PiTMqH0dNS2Q==";
      };
    };
    "@types/estree-0.0.39" = {
      name = "_at_types_slash_estree";
      packageName = "@types/estree";
      version = "0.0.39";
      src = fetchurl {
        url = "https://registry.npmjs.org/@types/estree/-/estree-0.0.39.tgz";
        sha512 =
          "EYNwp3bU+98cpU4lAWYYL7Zz+2gryWH1qbdDTidVd6hkiR6weksdbMadyXKXNPEkQFhXM+hVO9ZygomHXp+AIw==";
      };
    };
    "acorn-6.4.2" = {
      name = "acorn";
      packageName = "acorn";
      version = "6.4.2";
      src = fetchurl {
        url = "https://registry.npmjs.org/acorn/-/acorn-6.4.2.tgz";
        sha512 =
          "XtGIhXwF8YM8bJhGxG5kXgjkEuNGLTkoYqVE+KMR+aspr4KGYmKYg7yUe3KghyQ9yheNwLnjmzh/7+gfDBmHCQ==";
      };
    };
    "acorn-dynamic-import-4.0.0" = {
      name = "acorn-dynamic-import";
      packageName = "acorn-dynamic-import";
      version = "4.0.0";
      src = fetchurl {
        url =
          "https://registry.npmjs.org/acorn-dynamic-import/-/acorn-dynamic-import-4.0.0.tgz";
        sha512 =
          "d3OEjQV4ROpoflsnUA8HozoIR504TFxNivYEUi6uwz0IYhBkTDXGuWlNdMtybRt3nqVx/L6XqMt0FxkXuWKZhw==";
      };
    };
    "acorn-jsx-5.3.1" = {
      name = "acorn-jsx";
      packageName = "acorn-jsx";
      version = "5.3.1";
      src = fetchurl {
        url = "https://registry.npmjs.org/acorn-jsx/-/acorn-jsx-5.3.1.tgz";
        sha512 =
          "K0Ptm/47OKfQRpNQ2J/oIN/3QYiK6FwW+eJbILhsdxh2WTLdl+30o8aGdTbm5JbffpFFAg/g+zi1E+jvJha5ng==";
      };
    };
    "ansi-styles-3.2.1" = {
      name = "ansi-styles";
      packageName = "ansi-styles";
      version = "3.2.1";
      src = fetchurl {
        url = "https://registry.npmjs.org/ansi-styles/-/ansi-styles-3.2.1.tgz";
        sha512 =
          "VT0ZI6kZRdTh8YyJw3SMbYm/u+NqfsAxEpWO0Pf9sq8/e94WxxOpPKx9FR1FlyCtOVDNOQ+8ntlqFxiRc+r5qA==";
      };
    };
    "buble-0.20.0" = {
      name = "buble";
      packageName = "buble";
      version = "0.20.0";
      src = fetchurl {
        url = "https://registry.npmjs.org/buble/-/buble-0.20.0.tgz";
        sha512 =
          "/1gnaMQE8xvd5qsNBl+iTuyjJ9XxeaVxAMF86dQ4EyxFJOZtsgOS8Ra+7WHgZTam5IFDtt4BguN0sH0tVTKrOw==";
      };
    };
    "chalk-2.4.2" = {
      name = "chalk";
      packageName = "chalk";
      version = "2.4.2";
      src = fetchurl {
        url = "https://registry.npmjs.org/chalk/-/chalk-2.4.2.tgz";
        sha512 =
          "Mti+f9lpJNcwF4tWV8/OrTTtF1gZi+f8FqlyAdouralcFWFQWF2+NgCHShjkCb+IFBLq9buZwE1xckQU4peSuQ==";
      };
    };
    "color-convert-1.9.3" = {
      name = "color-convert";
      packageName = "color-convert";
      version = "1.9.3";
      src = fetchurl {
        url =
          "https://registry.npmjs.org/color-convert/-/color-convert-1.9.3.tgz";
        sha512 =
          "QfAUtd+vFdAtFQcC8CCyYt1fYWxSqAiK2cSD6zDB8N3cpsEBAvRxp9zOGg6G/SHHJYAT88/az/IuDGALsNVbGg==";
      };
    };
    "color-name-1.1.3" = {
      name = "color-name";
      packageName = "color-name";
      version = "1.1.3";
      src = fetchurl {
        url = "https://registry.npmjs.org/color-name/-/color-name-1.1.3.tgz";
        sha1 = "a7d0558bd89c42f795dd42328f740831ca53bc25";
      };
    };
    "escape-string-regexp-1.0.5" = {
      name = "escape-string-regexp";
      packageName = "escape-string-regexp";
      version = "1.0.5";
      src = fetchurl {
        url =
          "https://registry.npmjs.org/escape-string-regexp/-/escape-string-regexp-1.0.5.tgz";
        sha1 = "1b61c0562190a8dff6ae3bb2cf0200ca130b86d4";
      };
    };
    "estree-walker-1.0.1" = {
      name = "estree-walker";
      packageName = "estree-walker";
      version = "1.0.1";
      src = fetchurl {
        url =
          "https://registry.npmjs.org/estree-walker/-/estree-walker-1.0.1.tgz";
        sha512 =
          "1fMXF3YP4pZZVozF8j/ZLfvnR8NSIljt56UhbZ5PeeDmmGHpgpdwQt7ITlGvYaQukCvuBRMLEiKiYC+oeIg4cg==";
      };
    };
    "fsevents-2.3.2" = {
      name = "fsevents";
      packageName = "fsevents";
      version = "2.3.2";
      src = fetchurl {
        url = "https://registry.npmjs.org/fsevents/-/fsevents-2.3.2.tgz";
        sha512 =
          "xiqMQR4xAeHTuB9uWm+fFRcIOgKBMiOBP+eXiyT7jsgVCq1bkVygt00oASowB7EdtpOHaaPgKt812P9ab+DDKA==";
      };
    };
    "has-flag-3.0.0" = {
      name = "has-flag";
      packageName = "has-flag";
      version = "3.0.0";
      src = fetchurl {
        url = "https://registry.npmjs.org/has-flag/-/has-flag-3.0.0.tgz";
        sha1 = "b5d454dc2199ae225699f3467e5a07f3b955bafd";
      };
    };
    "jsesc-0.5.0" = {
      name = "jsesc";
      packageName = "jsesc";
      version = "0.5.0";
      src = fetchurl {
        url = "https://registry.npmjs.org/jsesc/-/jsesc-0.5.0.tgz";
        sha1 = "e7dee66e35d6fc16f710fe91d5cf69f70f08911d";
      };
    };
    "magic-string-0.25.7" = {
      name = "magic-string";
      packageName = "magic-string";
      version = "0.25.7";
      src = fetchurl {
        url =
          "https://registry.npmjs.org/magic-string/-/magic-string-0.25.7.tgz";
        sha512 =
          "4CrMT5DOHTDk4HYDlzmwu4FVCcIYI8gauveasrdCu2IKIFOJ3f0v/8MDGJCDL9oD2ppz/Av1b0Nj345H9M+XIA==";
      };
    };
    "minimist-1.2.5" = {
      name = "minimist";
      packageName = "minimist";
      version = "1.2.5";
      src = fetchurl {
        url = "https://registry.npmjs.org/minimist/-/minimist-1.2.5.tgz";
        sha512 =
          "FM9nNUYrRBAELZQT3xeZQ7fmMOBg6nWNmJKTcgsJeaLstP/UODVpGsr5OhXhhXg6f+qtJ8uiZ+PUxkDWcgIXLw==";
      };
    };
    "picomatch-2.2.3" = {
      name = "picomatch";
      packageName = "picomatch";
      version = "2.2.3";
      src = fetchurl {
        url = "https://registry.npmjs.org/picomatch/-/picomatch-2.2.3.tgz";
        sha512 =
          "KpELjfwcCDUb9PeigTs2mBJzXUPzAuP2oPcA989He8Rte0+YUAjw1JVedDhuTKPkHjSYzMN3npC9luThGYEKdg==";
      };
    };
    "regenerate-1.4.2" = {
      name = "regenerate";
      packageName = "regenerate";
      version = "1.4.2";
      src = fetchurl {
        url = "https://registry.npmjs.org/regenerate/-/regenerate-1.4.2.tgz";
        sha512 =
          "zrceR/XhGYU/d/opr2EKO7aRHUeiBI8qjtfHqADTwZd6Szfy16la6kqD0MIUs5z5hx6AaKa+PixpPrR289+I0A==";
      };
    };
    "regenerate-unicode-properties-8.2.0" = {
      name = "regenerate-unicode-properties";
      packageName = "regenerate-unicode-properties";
      version = "8.2.0";
      src = fetchurl {
        url =
          "https://registry.npmjs.org/regenerate-unicode-properties/-/regenerate-unicode-properties-8.2.0.tgz";
        sha512 =
          "F9DjY1vKLo/tPePDycuH3dn9H1OTPIkVD9Kz4LODu+F2C75mgjAJ7x/gwy6ZcSNRAAkhNlJSOHRe8k3p+K9WhA==";
      };
    };
    "regexpu-core-4.5.4" = {
      name = "regexpu-core";
      packageName = "regexpu-core";
      version = "4.5.4";
      src = fetchurl {
        url =
          "https://registry.npmjs.org/regexpu-core/-/regexpu-core-4.5.4.tgz";
        sha512 =
          "BtizvGtFQKGPUcTy56o3nk1bGRp4SZOTYrDtGNlqCQufptV5IkkLN6Emw+yunAJjzf+C9FQFtvq7IoA3+oMYHQ==";
      };
    };
    "regjsgen-0.5.2" = {
      name = "regjsgen";
      packageName = "regjsgen";
      version = "0.5.2";
      src = fetchurl {
        url = "https://registry.npmjs.org/regjsgen/-/regjsgen-0.5.2.tgz";
        sha512 =
          "OFFT3MfrH90xIW8OOSyUrk6QHD5E9JOTeGodiJeBS3J6IwlgzJMNE/1bZklWz5oTg+9dCMyEetclvCVXOPoN3A==";
      };
    };
    "regjsparser-0.6.9" = {
      name = "regjsparser";
      packageName = "regjsparser";
      version = "0.6.9";
      src = fetchurl {
        url = "https://registry.npmjs.org/regjsparser/-/regjsparser-0.6.9.tgz";
        sha512 =
          "ZqbNRz1SNjLAiYuwY0zoXW8Ne675IX5q+YHioAGbCw4X96Mjl2+dcX9B2ciaeyYjViDAfvIjFpQjJgLttTEERQ==";
      };
    };
    "sourcemap-codec-1.4.8" = {
      name = "sourcemap-codec";
      packageName = "sourcemap-codec";
      version = "1.4.8";
      src = fetchurl {
        url =
          "https://registry.npmjs.org/sourcemap-codec/-/sourcemap-codec-1.4.8.tgz";
        sha512 =
          "9NykojV5Uih4lgo5So5dtw+f0JgJX30KCNI8gwhz2J9A15wD0Ml6tjHKwf6fTSa6fAdVBdZeNOs9eJ71qCk8vA==";
      };
    };
    "supports-color-5.5.0" = {
      name = "supports-color";
      packageName = "supports-color";
      version = "5.5.0";
      src = fetchurl {
        url =
          "https://registry.npmjs.org/supports-color/-/supports-color-5.5.0.tgz";
        sha512 =
          "QjVjwdXIt408MIiAqCX4oUKsgU2EqAGzs2Ppkm4aQYbjm+ZEWEcW4SfFNTr4uMNZma0ey4f5lgLrkB0aX0QMow==";
      };
    };
    "unicode-canonical-property-names-ecmascript-1.0.4" = {
      name = "unicode-canonical-property-names-ecmascript";
      packageName = "unicode-canonical-property-names-ecmascript";
      version = "1.0.4";
      src = fetchurl {
        url =
          "https://registry.npmjs.org/unicode-canonical-property-names-ecmascript/-/unicode-canonical-property-names-ecmascript-1.0.4.tgz";
        sha512 =
          "jDrNnXWHd4oHiTZnx/ZG7gtUTVp+gCcTTKr8L0HjlwphROEW3+Him+IpvC+xcJEFegapiMZyZe02CyuOnRmbnQ==";
      };
    };
    "unicode-match-property-ecmascript-1.0.4" = {
      name = "unicode-match-property-ecmascript";
      packageName = "unicode-match-property-ecmascript";
      version = "1.0.4";
      src = fetchurl {
        url =
          "https://registry.npmjs.org/unicode-match-property-ecmascript/-/unicode-match-property-ecmascript-1.0.4.tgz";
        sha512 =
          "L4Qoh15vTfntsn4P1zqnHulG0LdXgjSO035fEpdtp6YxXhMT51Q6vgM5lYdG/5X3MjS+k/Y9Xw4SFCY9IkR0rg==";
      };
    };
    "unicode-match-property-value-ecmascript-1.2.0" = {
      name = "unicode-match-property-value-ecmascript";
      packageName = "unicode-match-property-value-ecmascript";
      version = "1.2.0";
      src = fetchurl {
        url =
          "https://registry.npmjs.org/unicode-match-property-value-ecmascript/-/unicode-match-property-value-ecmascript-1.2.0.tgz";
        sha512 =
          "wjuQHGQVofmSJv1uVISKLE5zO2rNGzM/KCYZch/QQvez7C1hUhBIuZ701fYXExuufJFMPhv2SyL8CyoIfMLbIQ==";
      };
    };
    "unicode-property-aliases-ecmascript-1.1.0" = {
      name = "unicode-property-aliases-ecmascript";
      packageName = "unicode-property-aliases-ecmascript";
      version = "1.1.0";
      src = fetchurl {
        url =
          "https://registry.npmjs.org/unicode-property-aliases-ecmascript/-/unicode-property-aliases-ecmascript-1.1.0.tgz";
        sha512 =
          "PqSoPh/pWetQ2phoj5RLiaqIk4kCNwoV3CI+LfGmWLKI3rE3kl1h59XpX2BjgDrmbxD9ARtQobPGU1SguCYuQg==";
      };
    };
  };
in {
  codemirror = nodeEnv.buildNodePackage {
    name = "codemirror";
    packageName = "codemirror";
    version = "5.61.0";
    src = fetchurl {
      url = "https://registry.npmjs.org/codemirror/-/codemirror-5.61.0.tgz";
      sha512 =
        "D3wYH90tYY1BsKlUe0oNj2JAhQ9TepkD51auk3N7q+4uz7A/cgJ5JsWHreT0PqieW1QhOuqxQ2reCXV1YXzecg==";
    };
    buildInputs = globalBuildInputs;
    meta = {
      description = "Full-featured in-browser code editor";
      homepage = "https://codemirror.net";
      license = "MIT";
    };
    production = true;
    bypassCache = true;
    reconstructLock = true;
  };
  rollup = nodeEnv.buildNodePackage {
    name = "rollup";
    packageName = "rollup";
    version = "2.46.0";
    src = fetchurl {
      url = "https://registry.npmjs.org/rollup/-/rollup-2.46.0.tgz";
      sha512 =
        "qPGoUBNl+Z8uNu0z7pD3WPTABWRbcOwIrO/5ccDJzmrtzn0LVf6Lj91+L5CcWhXl6iWf23FQ6m8Jkl2CmN1O7Q==";
    };
    dependencies = [ sources."fsevents-2.3.2" ];
    buildInputs = globalBuildInputs;
    meta = {
      description = "Next-generation ES module bundler";
      homepage = "https://rollupjs.org/";
      license = "MIT";
    };
    production = true;
    bypassCache = true;
    reconstructLock = true;
  };
  "@rollup/plugin-buble" = nodeEnv.buildNodePackage {
    name = "_at_rollup_slash_plugin-buble";
    packageName = "@rollup/plugin-buble";
    version = "0.21.3";
    src = fetchurl {
      url =
        "https://registry.npmjs.org/@rollup/plugin-buble/-/plugin-buble-0.21.3.tgz";
      sha512 =
        "Iv8cCuFPnMdqV4pcyU+OrfjOfagPArRQ1PyQjx5KgHk3dARedI+8PNTLSMpJts0lQJr8yF2pAU4GxpxCBJ9HYw==";
    };
    dependencies = [
      sources."@rollup/pluginutils-3.1.0"
      sources."@types/buble-0.19.2"
      sources."@types/estree-0.0.39"
      sources."acorn-6.4.2"
      sources."acorn-dynamic-import-4.0.0"
      sources."acorn-jsx-5.3.1"
      sources."ansi-styles-3.2.1"
      sources."buble-0.20.0"
      sources."chalk-2.4.2"
      sources."color-convert-1.9.3"
      sources."color-name-1.1.3"
      sources."escape-string-regexp-1.0.5"
      sources."estree-walker-1.0.1"
      sources."has-flag-3.0.0"
      sources."jsesc-0.5.0"
      sources."magic-string-0.25.7"
      sources."minimist-1.2.5"
      sources."picomatch-2.2.3"
      sources."regenerate-1.4.2"
      sources."regenerate-unicode-properties-8.2.0"
      sources."regexpu-core-4.5.4"
      sources."regjsgen-0.5.2"
      sources."regjsparser-0.6.9"
      sources."sourcemap-codec-1.4.8"
      sources."supports-color-5.5.0"
      sources."unicode-canonical-property-names-ecmascript-1.0.4"
      sources."unicode-match-property-ecmascript-1.0.4"
      sources."unicode-match-property-value-ecmascript-1.2.0"
      sources."unicode-property-aliases-ecmascript-1.1.0"
    ];
    buildInputs = globalBuildInputs;
    meta = {
      description = "Compile ES2015 with buble";
      homepage =
        "https://github.com/rollup/plugins/tree/master/packages/buble/#readme";
      license = "MIT";
    };
    production = true;
    bypassCache = true;
    reconstructLock = true;
  };
}
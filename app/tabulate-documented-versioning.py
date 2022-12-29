from bs4 import BeautifulSoup
import argparse
import csv
import math
import os
import pandas
import re
import requests

parseRecordHTML = lambda r: BeautifulSoup(r.content, "html.parser")

patternMatchHTML = (
    lambda pattern, tag: None if tag is None else pattern.search(tag.text).group(1)
)

patternMatchLib = (
    lambda pattern, tag: (None, None)
    if tag is None
    else (pattern.search(tag.text).group(1)[:-1], pattern.search(tag.text).group(2))
)

GHC_BOUND_MAJOR = 7

GHC_BOUND_MINOR = 0

GHC_BOUND_PATCH = 0

GHC_BOUND_VALID = (
    lambda x, y, z: x > GHC_BOUND_MAJOR
    or (x == GHC_BOUND_MAJOR and y > GHC_BOUND_MINOR)
    or (x == GHC_BOUND_MAJOR and y == GHC_BOUND_MINOR and z >= GHC_BOUND_PATCH)
)

GHC_FRAME_INDEX = "ghc"

GHC_FUSE_DIGITS = lambda x, y, z: ".".join([str(x), str(y), str(z)])

LIB_PKG_PATTERN = re.compile(r"([\w-]+)")

LIB_REF_PATTERN = re.compile(r"([\w-]+)-([\d.]+)(?::.*)?$")

LIB_VER_PATTERN = re.compile(r"([\w-]+)-([\d.]+)?")


def extractLibraryVersion(string):

    match = LIB_VER_PATTERN.search(string)
    lib = match.group(1) if match is not None else None
    ver = match.group(2) if match is not None else None

    if lib is None or ver is None:
        match = LIB_REF_PATTERN.search(string)
        lib = match.group(1) if match is not None and lib is None else None
        ver = match.group(2) if match is not None and ver is None else None

    if lib is None:
        match = LIB_PKG_PATTERN.search(string)
        lib = match.group(1) if match is not None else None

    return lib, ver


def constructURL(major, minor, patch):
    versionStr = ".".join([str(major), str(minor), str(patch)])
    return "".join(
        [
            "https://downloads.haskell.org/~ghc/",
            versionStr,
            "/docs/html/users_guide/",
            versionStr,
            "-notes.html",
        ]
    )


def queryCompilerVersionURLs():
    def ghcVersionNumbers(txt):
        match = versionPattern.search(txt)
        major = int(match.group(1))
        minor = int(match.group(2))
        patch = int(match.group(3))
        return major, minor, patch

    def ghcDocumentationURL(major, minor, patch):
        ghcStr = GHC_FUSE_DIGITS(major, minor, patch)
        return "/".join([indexURL, ghcStr, "docs", "html", "users_guide", "index.html"])

    def ghcCoreLibrariesURL(docURL):
        jointPattern = re.compile(r"\b(?:Libraries|Included libraries)\b")
        docsResponse = requests.get(docURL)
        docsPageHTML = parseRecordHTML(docsResponse)
        docsNextLink = docsPageHTML.find("a", text=jointPattern)
        docsRelative = docsNextLink["href"]
        return "/".join(docURL.split("/")[:-1] + [docsRelative])

    def ghcPackagingInfoURL(major, minor, patch):
        ghcStr = GHC_FUSE_DIGITS(major, minor, patch)
        return "/".join([indexURL, ghcStr, "docs", "libraries", "index.html"])

    versionPattern = re.compile(r"(\d+)\.(\d+)\.(\d+)/")

    indexURL = "https://downloads.haskell.org/~ghc"
    response = requests.get(indexURL)
    ghcLinks = parseRecordHTML(response).find_all("a", text=versionPattern)

    ghcVersionsFound = []

    for anchor in ghcLinks:
        ghcVersionsFound.append(ghcVersionNumbers(anchor.text))

    ghcVersionsFound.sort()

    for ghcMajor, ghcMinor, ghcPatch in ghcVersionsFound:
        if GHC_BOUND_VALID(ghcMajor, ghcMinor, ghcPatch):
            ghcDocURL = ghcDocumentationURL(ghcMajor, ghcMinor, ghcPatch)
            ghcLibURL = ghcCoreLibrariesURL(ghcDocURL)

            yield ghcMajor, ghcMinor, ghcPatch, ghcLibURL


def queryCompilerVersionURLs():
    def ghcVersionNumbers(txt):
        match = versionPattern.search(txt)
        major = int(match.group(1))
        minor = int(match.group(2))
        patch = int(match.group(3))
        return major, minor, patch

    def ghcPackagingInfoURL(major, minor, patch):
        ghcLabel = GHC_FUSE_DIGITS(major, minor, patch)
        buildURL = lambda x: "/".join(
            [indexURL, ghcLabel, "docs"] + x + ["libraries", "index.html"]
        )
        firstURL = buildURL(["html"])
        finalURL = buildURL([])
        response = requests.get(firstURL)
        return firstURL if response.status_code == 200 else finalURL

    versionPattern = re.compile(r"(\d+)\.(\d+)\.(\d+)/")

    indexURL = "https://downloads.haskell.org/~ghc"
    response = requests.get(indexURL)
    ghcLinks = parseRecordHTML(response).find_all("a", text=versionPattern)

    ghcVersionsFound = []

    for anchor in ghcLinks:
        ghcVersionsFound.append(ghcVersionNumbers(anchor.text))

    ghcVersionsFound.sort()

    for ghcMajor, ghcMinor, ghcPatch in ghcVersionsFound:
        if GHC_BOUND_VALID(ghcMajor, ghcMinor, ghcPatch):
            ghcLibURL = ghcPackagingInfoURL(ghcMajor, ghcMinor, ghcPatch)

            yield ghcMajor, ghcMinor, ghcPatch, ghcLibURL


def gatherCoreLibraryVersions(libsURL):

    scrapingRoutines = [scrapeLibraryVersions_I, scrapeLibraryVersions_II]

    result = None

    for scraper in scrapingRoutines:
        result = scraper(libsURL)
        if result is not None:
            break

    return result


def scrapeLibraryVersions_I(libsURL):

    libsVerFound = dict()
    libsModLinks = collectAnchors_1(libsURL)

    if libsModLinks is None:
        return None

    for anchor in libsModLinks:

        # Check for existence of sibling label
        sibling = locateSibling_1(anchor)
        # anchor.parent.find_next_sibling("span", class_="package")
        if sibling is not None:

            # Use the available labeling
            lib, ver = extractLibraryVersion(sibling.text)

            if lib is not None and ver is not None:
                if lib not in libsVerFound:
                    # print("> PUT", lib, ver)
                    libsVerFound[lib] = ver
                continue

        # With no sibling label, check the link
        target = anchor["href"]

        # The link contains a *probable* library name
        lib, _ = extractLibraryVersion(target)

        # Skip as we already have this library version
        if lib is not None and lib in libsVerFound:
            continue

        # We must follow the link, then extract the library name and version
        newURL = "/".join(libsURL.split("/")[:-1] + [target])
        # print("GET", newURL)

        moduleResponse = requests.get(newURL)
        modulePageHTML = parseRecordHTML(moduleResponse)
        moduleLabeling = modulePageHTML.find(text=LIB_REF_PATTERN)

        if moduleLabeling is None:
            continue

        lib, ver = extractLibraryVersion(moduleLabeling.text)
        if lib is not None and ver is not None:
            if lib not in libsVerFound:
                # print("/ PUT", lib, ver)
                libsVerFound[lib] = ver
            continue

    return dict(sorted(libsVerFound.items()))


def scrapeLibraryVersions_II(libsURL):

    libsVerFound = dict()
    libsModLinks = collectAnchors_2(libsURL)

    if libsModLinks is None:
        return None

    for anchor in libsModLinks:

        # Check for existence of sibling label
        sibling = locateSibling_2(anchor)
        # *_, sibling = anchor.parent.next_siblings
        if sibling is not None:

            # Use the available labeling if possible
            lib, ver = extractLibraryVersion(sibling.text)
            if lib is not None:
                if lib in libsVerFound:
                    continue
                else:
                    if ver is not None:
                        # print("> PUT", lib, ver)
                        libsVerFound[lib] = ver

        # With no sibling label, check the link
        target = anchor["href"]

        # The link contains a *probable* library name
        lib, _ = extractLibraryVersion(target)

        # Skip as we already have this library version
        if lib is not None and lib in libsVerFound:
            continue

        # We must follow the link, then extract the library name and version
        newURL = "/".join(libsURL.split("/")[:-1] + [target])
        # print("GET", newURL)

        moduleResponse = requests.get(newURL)
        modulePageHTML = parseRecordHTML(moduleResponse)
        moduleLabeling = modulePageHTML.find(text=LIB_REF_PATTERN)

        if moduleLabeling is None:
            continue

        lib, ver = extractLibraryVersion(moduleLabeling.text)
        if lib is not None and ver is not None:
            if lib not in libsVerFound:
                # print("/ PUT", lib, ver)
                libsVerFound[lib] = ver
            continue

    return dict(sorted(libsVerFound.items()))


def collectAnchors_1(libsURL):

    libsResponse = requests.get(libsURL)
    libsPageHTML = parseRecordHTML(libsResponse)
    libsModArray = libsPageHTML.find(text=["Modules", "Packages"]).find_parent(
        id="module-list"
    )
    libsModLinks = None if libsModArray is None else libsModArray.find_all("a")
    libsModLinks = (
        libsModLinks
        if libsModLinks is not None and len(list(libsModLinks)) > 0
        else None
    )

    return libsModLinks


def collectAnchors_2(libsURL):

    libsResponse = requests.get(libsURL)
    libsPageHTML = parseRecordHTML(libsResponse)
    libsModTitle = libsPageHTML.find(class_="section1", text="Modules")
    libsModUpper = libsModTitle.parent.parent
    libsModTable = libsModUpper.find("table", class_="vanilla2")
    libsModLinks = libsModTable.find_all("a")
    libsModLinks = (
        libsModLinks
        if libsModLinks is not None and len(list(libsModLinks)) > 0
        else None
    )

    return libsModLinks


def locateSibling_1(a):
    p = None if a is None else a.parent
    s = None if p is None else p.find_next_sibling("span", class_="package")
    return s


def locateSibling_2(a):
    p = None if a is None else a.parent
    *_, s = [None] if p is None else p.next_siblings
    return s


def finalizeFrame(dictionaries):
    def explodeVersion(ver):
        strs = ver.split(".") if type(ver) == str else []
        part = lambda i: int(strs[i]) if len(strs) > i else None
        nums = [part(0), part(1), part(2), part(3)]
        vals = pandas.Series(nums, dtype=pandas.UInt16Dtype())
        return vals[0], vals[1], vals[2], vals[3]

    columnIndex = lambda x: lambda i: x + "_" + ["MAJOR", "MINOR", "PATCH", "POINT"][i]
    ghcLabeling = columnIndex(GHC_FRAME_INDEX)
    ghcOrdering = list(map(ghcLabeling, range(4)))
    sortingSpec = {
        "by": ghcOrdering,
        "ascending": True,
        "inplace": True,
        "na_position": "first",
    }

    df = pandas.DataFrame(dictionaries)
    df = df.reindex(sorted(df.columns), axis=1)
    df.insert(0, GHC_FRAME_INDEX, df.pop(GHC_FRAME_INDEX))

    for lib in df.columns:
        f = columnIndex(lib)
        df[f(0)], df[f(1)], df[f(2)], df[f(3)] = zip(*df[lib].map(explodeVersion))
        df.drop(lib, axis=1, inplace=True)

    df.sort_values(**sortingSpec)

    return df


def commandLineArgumentParser():

    specAbstract = {
        "prog": "GHC Versioning Curator",
        "description": "Querries haskell.org for the core library versions distributed with each compiler version",
        "epilog": "Used to power a cabal tooling library",
    }

    specFilepath = {
        "default": "data/distribution-versioning.csv",
        "metavar": "FILE",
        "help": "Output filepath for CSV results",
    }

    specVerbosed = {
        "action": "store_true",
        "default": False,
    }

    specVersions = {
        "action": "version",
        "version": "%(prog)s 1.0.0",
    }

    clap = argparse.ArgumentParser(**specAbstract)
    clap.add_argument("outputCSV", **specFilepath)
    clap.add_argument("-v", "--verbose", **specVerbosed)
    clap.add_argument("--version", **specVersions)

    return clap


def writeOutResults(frame, path):

    formatCSV = {
        "doublequote": True,
        "header": True,
        "index": False,
        "quoting": csv.QUOTE_ALL,
    }

    frame.to_csv(path, **formatCSV)

    rowStr = str(frame.shape[0])
    colStr = str(frame.shape[1] // 4)

    report("")
    report("Tabulated documented distribution versioning for")
    reportItem("GHC compiler versions:\t" + rowStr)
    reportItem("Core library versions:\t" + colStr)
    report("")
    report("CSV written out:")
    reportItem(path)
    report("")


SILENT = True

report = None
reportItem = None


def defineReporting(loud):
    if loud:

        def _report(tokens):
            print(tokens)

        def _reportItem(item):
            print("  -", item)

    else:
        _report = lambda *x: None
        _reportItem = lambda *x: None

    global report
    report = _report

    global reportItem
    reportItem = _reportItem


def main():

    args = commandLineArgumentParser().parse_args()

    defineReporting(args.verbose)

    report("Gathered versioning information for GHC versions:")

    compilerCoreLibraryVersions = []

    for major, minor, patch, URL in queryCompilerVersionURLs():
        ghcVersionString = GHC_FUSE_DIGITS(major, minor, patch)
        libVersionsFound = gatherCoreLibraryVersions(URL)
        libVersionsFound[GHC_FRAME_INDEX] = ghcVersionString
        compilerCoreLibraryVersions.append(libVersionsFound)
        reportItem(GHC_FRAME_INDEX + "-" + ghcVersionString)

    ghcVersioningFrame = finalizeFrame(compilerCoreLibraryVersions)
    writeOutResults(ghcVersioningFrame, args.outputCSV)


if __name__ == "__main__":
    main()

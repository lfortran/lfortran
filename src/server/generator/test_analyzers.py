import json
import sys
import traceback
from pathlib import Path

from llanguage_server.lsp.auxiliary_schema import AUXILIARY_SCHEMA
from llanguage_server.lsp.visitors import (LspDependencyExtractor, LspIndexer,
                                           LspStructFieldExpander,
                                           LspSymbolResolver,
                                           LspAnalysisPipeline)

EXIT_SUCCESS: int = 0
EXIT_FAILURE: int = 1


def main() -> int:
    with open(Path(__file__).parent / "metaModel.json") as f:
        schema = json.loads(f.read())
    for key, value in AUXILIARY_SCHEMA.items():
        schema[key].extend(value)
    pipeline = LspAnalysisPipeline(schema)
    pipeline.run()
    indexer = pipeline.indexer
    dependency_extractor = pipeline.dependency_extractor
    struct_field_expander = pipeline.struct_field_expander
    symbol_resolver = pipeline.symbol_resolver
    import IPython; IPython.embed()
    return EXIT_SUCCESS


if __name__ == "__main__":
    try:
        exit_code = main()
        sys.exit(exit_code)
    except SystemExit as system_exit:
        if system_exit.code is not None:
            if system_exit.code != 0:
                import pudb; pudb.post_mortem()
                traceback.print_exc()
            sys.exit(system_exit.code)
    except Exception as e:
        import pudb; pudb.post_mortem()
        print("Rescued unhandled exception:", file=sys.stderr)
        traceback.print_exc()
        sys.exit(EXIT_FAILURE)

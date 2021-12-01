{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Comprehend.Types.DocumentReaderConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Comprehend.Types.DocumentReaderConfig where

import Amazonka.Comprehend.Types.DocumentReadAction
import Amazonka.Comprehend.Types.DocumentReadFeatureTypes
import Amazonka.Comprehend.Types.DocumentReadMode
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | The input properties for a topic detection job.
--
-- /See:/ 'newDocumentReaderConfig' smart constructor.
data DocumentReaderConfig = DocumentReaderConfig'
  { -- | Specifies how the text in an input file should be processed:
    featureTypes :: Prelude.Maybe (Prelude.NonEmpty DocumentReadFeatureTypes),
    -- | This enum field provides two values:
    --
    -- -   @SERVICE_DEFAULT@ - use service defaults for Document reading. For
    --     Digital PDF it would mean using an internal parser instead of
    --     Textract APIs
    --
    -- -   @FORCE_DOCUMENT_READ_ACTION@ - Always use specified action for
    --     DocumentReadAction, including Digital PDF.
    documentReadMode :: Prelude.Maybe DocumentReadMode,
    -- | This enum field will start with two values which will apply to PDFs:
    --
    -- -   @TEXTRACT_DETECT_DOCUMENT_TEXT@ - The service calls
    --     DetectDocumentText for PDF documents per page.
    --
    -- -   @TEXTRACT_ANALYZE_DOCUMENT@ - The service calls AnalyzeDocument for
    --     PDF documents per page.
    documentReadAction :: DocumentReadAction
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DocumentReaderConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'featureTypes', 'documentReaderConfig_featureTypes' - Specifies how the text in an input file should be processed:
--
-- 'documentReadMode', 'documentReaderConfig_documentReadMode' - This enum field provides two values:
--
-- -   @SERVICE_DEFAULT@ - use service defaults for Document reading. For
--     Digital PDF it would mean using an internal parser instead of
--     Textract APIs
--
-- -   @FORCE_DOCUMENT_READ_ACTION@ - Always use specified action for
--     DocumentReadAction, including Digital PDF.
--
-- 'documentReadAction', 'documentReaderConfig_documentReadAction' - This enum field will start with two values which will apply to PDFs:
--
-- -   @TEXTRACT_DETECT_DOCUMENT_TEXT@ - The service calls
--     DetectDocumentText for PDF documents per page.
--
-- -   @TEXTRACT_ANALYZE_DOCUMENT@ - The service calls AnalyzeDocument for
--     PDF documents per page.
newDocumentReaderConfig ::
  -- | 'documentReadAction'
  DocumentReadAction ->
  DocumentReaderConfig
newDocumentReaderConfig pDocumentReadAction_ =
  DocumentReaderConfig'
    { featureTypes =
        Prelude.Nothing,
      documentReadMode = Prelude.Nothing,
      documentReadAction = pDocumentReadAction_
    }

-- | Specifies how the text in an input file should be processed:
documentReaderConfig_featureTypes :: Lens.Lens' DocumentReaderConfig (Prelude.Maybe (Prelude.NonEmpty DocumentReadFeatureTypes))
documentReaderConfig_featureTypes = Lens.lens (\DocumentReaderConfig' {featureTypes} -> featureTypes) (\s@DocumentReaderConfig' {} a -> s {featureTypes = a} :: DocumentReaderConfig) Prelude.. Lens.mapping Lens.coerced

-- | This enum field provides two values:
--
-- -   @SERVICE_DEFAULT@ - use service defaults for Document reading. For
--     Digital PDF it would mean using an internal parser instead of
--     Textract APIs
--
-- -   @FORCE_DOCUMENT_READ_ACTION@ - Always use specified action for
--     DocumentReadAction, including Digital PDF.
documentReaderConfig_documentReadMode :: Lens.Lens' DocumentReaderConfig (Prelude.Maybe DocumentReadMode)
documentReaderConfig_documentReadMode = Lens.lens (\DocumentReaderConfig' {documentReadMode} -> documentReadMode) (\s@DocumentReaderConfig' {} a -> s {documentReadMode = a} :: DocumentReaderConfig)

-- | This enum field will start with two values which will apply to PDFs:
--
-- -   @TEXTRACT_DETECT_DOCUMENT_TEXT@ - The service calls
--     DetectDocumentText for PDF documents per page.
--
-- -   @TEXTRACT_ANALYZE_DOCUMENT@ - The service calls AnalyzeDocument for
--     PDF documents per page.
documentReaderConfig_documentReadAction :: Lens.Lens' DocumentReaderConfig DocumentReadAction
documentReaderConfig_documentReadAction = Lens.lens (\DocumentReaderConfig' {documentReadAction} -> documentReadAction) (\s@DocumentReaderConfig' {} a -> s {documentReadAction = a} :: DocumentReaderConfig)

instance Core.FromJSON DocumentReaderConfig where
  parseJSON =
    Core.withObject
      "DocumentReaderConfig"
      ( \x ->
          DocumentReaderConfig'
            Prelude.<$> (x Core..:? "FeatureTypes")
            Prelude.<*> (x Core..:? "DocumentReadMode")
            Prelude.<*> (x Core..: "DocumentReadAction")
      )

instance Prelude.Hashable DocumentReaderConfig where
  hashWithSalt salt' DocumentReaderConfig' {..} =
    salt' `Prelude.hashWithSalt` documentReadAction
      `Prelude.hashWithSalt` documentReadMode
      `Prelude.hashWithSalt` featureTypes

instance Prelude.NFData DocumentReaderConfig where
  rnf DocumentReaderConfig' {..} =
    Prelude.rnf featureTypes
      `Prelude.seq` Prelude.rnf documentReadAction
      `Prelude.seq` Prelude.rnf documentReadMode

instance Core.ToJSON DocumentReaderConfig where
  toJSON DocumentReaderConfig' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("FeatureTypes" Core..=) Prelude.<$> featureTypes,
            ("DocumentReadMode" Core..=)
              Prelude.<$> documentReadMode,
            Prelude.Just
              ("DocumentReadAction" Core..= documentReadAction)
          ]
      )

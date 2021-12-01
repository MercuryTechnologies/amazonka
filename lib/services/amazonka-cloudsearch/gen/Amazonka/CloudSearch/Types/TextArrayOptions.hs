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
-- Module      : Amazonka.CloudSearch.Types.TextArrayOptions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudSearch.Types.TextArrayOptions where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Options for a field that contains an array of text strings. Present if
-- @IndexFieldType@ specifies the field is of type @text-array@. A
-- @text-array@ field is always searchable. All options are enabled by
-- default.
--
-- /See:/ 'newTextArrayOptions' smart constructor.
data TextArrayOptions = TextArrayOptions'
  { -- | A list of source fields to map to the field.
    sourceFields :: Prelude.Maybe Prelude.Text,
    -- | Whether the contents of the field can be returned in the search results.
    returnEnabled :: Prelude.Maybe Prelude.Bool,
    -- | The name of an analysis scheme for a @text-array@ field.
    analysisScheme :: Prelude.Maybe Prelude.Text,
    -- | Whether highlights can be returned for the field.
    highlightEnabled :: Prelude.Maybe Prelude.Bool,
    -- | A value to use for the field if the field isn\'t specified for a
    -- document.
    defaultValue :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TextArrayOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sourceFields', 'textArrayOptions_sourceFields' - A list of source fields to map to the field.
--
-- 'returnEnabled', 'textArrayOptions_returnEnabled' - Whether the contents of the field can be returned in the search results.
--
-- 'analysisScheme', 'textArrayOptions_analysisScheme' - The name of an analysis scheme for a @text-array@ field.
--
-- 'highlightEnabled', 'textArrayOptions_highlightEnabled' - Whether highlights can be returned for the field.
--
-- 'defaultValue', 'textArrayOptions_defaultValue' - A value to use for the field if the field isn\'t specified for a
-- document.
newTextArrayOptions ::
  TextArrayOptions
newTextArrayOptions =
  TextArrayOptions'
    { sourceFields = Prelude.Nothing,
      returnEnabled = Prelude.Nothing,
      analysisScheme = Prelude.Nothing,
      highlightEnabled = Prelude.Nothing,
      defaultValue = Prelude.Nothing
    }

-- | A list of source fields to map to the field.
textArrayOptions_sourceFields :: Lens.Lens' TextArrayOptions (Prelude.Maybe Prelude.Text)
textArrayOptions_sourceFields = Lens.lens (\TextArrayOptions' {sourceFields} -> sourceFields) (\s@TextArrayOptions' {} a -> s {sourceFields = a} :: TextArrayOptions)

-- | Whether the contents of the field can be returned in the search results.
textArrayOptions_returnEnabled :: Lens.Lens' TextArrayOptions (Prelude.Maybe Prelude.Bool)
textArrayOptions_returnEnabled = Lens.lens (\TextArrayOptions' {returnEnabled} -> returnEnabled) (\s@TextArrayOptions' {} a -> s {returnEnabled = a} :: TextArrayOptions)

-- | The name of an analysis scheme for a @text-array@ field.
textArrayOptions_analysisScheme :: Lens.Lens' TextArrayOptions (Prelude.Maybe Prelude.Text)
textArrayOptions_analysisScheme = Lens.lens (\TextArrayOptions' {analysisScheme} -> analysisScheme) (\s@TextArrayOptions' {} a -> s {analysisScheme = a} :: TextArrayOptions)

-- | Whether highlights can be returned for the field.
textArrayOptions_highlightEnabled :: Lens.Lens' TextArrayOptions (Prelude.Maybe Prelude.Bool)
textArrayOptions_highlightEnabled = Lens.lens (\TextArrayOptions' {highlightEnabled} -> highlightEnabled) (\s@TextArrayOptions' {} a -> s {highlightEnabled = a} :: TextArrayOptions)

-- | A value to use for the field if the field isn\'t specified for a
-- document.
textArrayOptions_defaultValue :: Lens.Lens' TextArrayOptions (Prelude.Maybe Prelude.Text)
textArrayOptions_defaultValue = Lens.lens (\TextArrayOptions' {defaultValue} -> defaultValue) (\s@TextArrayOptions' {} a -> s {defaultValue = a} :: TextArrayOptions)

instance Core.FromXML TextArrayOptions where
  parseXML x =
    TextArrayOptions'
      Prelude.<$> (x Core..@? "SourceFields")
      Prelude.<*> (x Core..@? "ReturnEnabled")
      Prelude.<*> (x Core..@? "AnalysisScheme")
      Prelude.<*> (x Core..@? "HighlightEnabled")
      Prelude.<*> (x Core..@? "DefaultValue")

instance Prelude.Hashable TextArrayOptions where
  hashWithSalt salt' TextArrayOptions' {..} =
    salt' `Prelude.hashWithSalt` defaultValue
      `Prelude.hashWithSalt` highlightEnabled
      `Prelude.hashWithSalt` analysisScheme
      `Prelude.hashWithSalt` returnEnabled
      `Prelude.hashWithSalt` sourceFields

instance Prelude.NFData TextArrayOptions where
  rnf TextArrayOptions' {..} =
    Prelude.rnf sourceFields
      `Prelude.seq` Prelude.rnf defaultValue
      `Prelude.seq` Prelude.rnf highlightEnabled
      `Prelude.seq` Prelude.rnf analysisScheme
      `Prelude.seq` Prelude.rnf returnEnabled

instance Core.ToQuery TextArrayOptions where
  toQuery TextArrayOptions' {..} =
    Prelude.mconcat
      [ "SourceFields" Core.=: sourceFields,
        "ReturnEnabled" Core.=: returnEnabled,
        "AnalysisScheme" Core.=: analysisScheme,
        "HighlightEnabled" Core.=: highlightEnabled,
        "DefaultValue" Core.=: defaultValue
      ]

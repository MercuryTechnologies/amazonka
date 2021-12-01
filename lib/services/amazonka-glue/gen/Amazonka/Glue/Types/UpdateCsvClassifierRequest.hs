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
-- Module      : Amazonka.Glue.Types.UpdateCsvClassifierRequest
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.UpdateCsvClassifierRequest where

import qualified Amazonka.Core as Core
import Amazonka.Glue.Types.CsvHeaderOption
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Specifies a custom CSV classifier to be updated.
--
-- /See:/ 'newUpdateCsvClassifierRequest' smart constructor.
data UpdateCsvClassifierRequest = UpdateCsvClassifierRequest'
  { -- | A custom symbol to denote what combines content into a single column
    -- value. It must be different from the column delimiter.
    quoteSymbol :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether the CSV file contains a header.
    containsHeader :: Prelude.Maybe CsvHeaderOption,
    -- | Specifies not to trim values before identifying the type of column
    -- values. The default value is true.
    disableValueTrimming :: Prelude.Maybe Prelude.Bool,
    -- | A list of strings representing column names.
    header :: Prelude.Maybe [Prelude.Text],
    -- | Enables the processing of files that contain only one column.
    allowSingleColumn :: Prelude.Maybe Prelude.Bool,
    -- | A custom symbol to denote what separates each column entry in the row.
    delimiter :: Prelude.Maybe Prelude.Text,
    -- | The name of the classifier.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateCsvClassifierRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'quoteSymbol', 'updateCsvClassifierRequest_quoteSymbol' - A custom symbol to denote what combines content into a single column
-- value. It must be different from the column delimiter.
--
-- 'containsHeader', 'updateCsvClassifierRequest_containsHeader' - Indicates whether the CSV file contains a header.
--
-- 'disableValueTrimming', 'updateCsvClassifierRequest_disableValueTrimming' - Specifies not to trim values before identifying the type of column
-- values. The default value is true.
--
-- 'header', 'updateCsvClassifierRequest_header' - A list of strings representing column names.
--
-- 'allowSingleColumn', 'updateCsvClassifierRequest_allowSingleColumn' - Enables the processing of files that contain only one column.
--
-- 'delimiter', 'updateCsvClassifierRequest_delimiter' - A custom symbol to denote what separates each column entry in the row.
--
-- 'name', 'updateCsvClassifierRequest_name' - The name of the classifier.
newUpdateCsvClassifierRequest ::
  -- | 'name'
  Prelude.Text ->
  UpdateCsvClassifierRequest
newUpdateCsvClassifierRequest pName_ =
  UpdateCsvClassifierRequest'
    { quoteSymbol =
        Prelude.Nothing,
      containsHeader = Prelude.Nothing,
      disableValueTrimming = Prelude.Nothing,
      header = Prelude.Nothing,
      allowSingleColumn = Prelude.Nothing,
      delimiter = Prelude.Nothing,
      name = pName_
    }

-- | A custom symbol to denote what combines content into a single column
-- value. It must be different from the column delimiter.
updateCsvClassifierRequest_quoteSymbol :: Lens.Lens' UpdateCsvClassifierRequest (Prelude.Maybe Prelude.Text)
updateCsvClassifierRequest_quoteSymbol = Lens.lens (\UpdateCsvClassifierRequest' {quoteSymbol} -> quoteSymbol) (\s@UpdateCsvClassifierRequest' {} a -> s {quoteSymbol = a} :: UpdateCsvClassifierRequest)

-- | Indicates whether the CSV file contains a header.
updateCsvClassifierRequest_containsHeader :: Lens.Lens' UpdateCsvClassifierRequest (Prelude.Maybe CsvHeaderOption)
updateCsvClassifierRequest_containsHeader = Lens.lens (\UpdateCsvClassifierRequest' {containsHeader} -> containsHeader) (\s@UpdateCsvClassifierRequest' {} a -> s {containsHeader = a} :: UpdateCsvClassifierRequest)

-- | Specifies not to trim values before identifying the type of column
-- values. The default value is true.
updateCsvClassifierRequest_disableValueTrimming :: Lens.Lens' UpdateCsvClassifierRequest (Prelude.Maybe Prelude.Bool)
updateCsvClassifierRequest_disableValueTrimming = Lens.lens (\UpdateCsvClassifierRequest' {disableValueTrimming} -> disableValueTrimming) (\s@UpdateCsvClassifierRequest' {} a -> s {disableValueTrimming = a} :: UpdateCsvClassifierRequest)

-- | A list of strings representing column names.
updateCsvClassifierRequest_header :: Lens.Lens' UpdateCsvClassifierRequest (Prelude.Maybe [Prelude.Text])
updateCsvClassifierRequest_header = Lens.lens (\UpdateCsvClassifierRequest' {header} -> header) (\s@UpdateCsvClassifierRequest' {} a -> s {header = a} :: UpdateCsvClassifierRequest) Prelude.. Lens.mapping Lens.coerced

-- | Enables the processing of files that contain only one column.
updateCsvClassifierRequest_allowSingleColumn :: Lens.Lens' UpdateCsvClassifierRequest (Prelude.Maybe Prelude.Bool)
updateCsvClassifierRequest_allowSingleColumn = Lens.lens (\UpdateCsvClassifierRequest' {allowSingleColumn} -> allowSingleColumn) (\s@UpdateCsvClassifierRequest' {} a -> s {allowSingleColumn = a} :: UpdateCsvClassifierRequest)

-- | A custom symbol to denote what separates each column entry in the row.
updateCsvClassifierRequest_delimiter :: Lens.Lens' UpdateCsvClassifierRequest (Prelude.Maybe Prelude.Text)
updateCsvClassifierRequest_delimiter = Lens.lens (\UpdateCsvClassifierRequest' {delimiter} -> delimiter) (\s@UpdateCsvClassifierRequest' {} a -> s {delimiter = a} :: UpdateCsvClassifierRequest)

-- | The name of the classifier.
updateCsvClassifierRequest_name :: Lens.Lens' UpdateCsvClassifierRequest Prelude.Text
updateCsvClassifierRequest_name = Lens.lens (\UpdateCsvClassifierRequest' {name} -> name) (\s@UpdateCsvClassifierRequest' {} a -> s {name = a} :: UpdateCsvClassifierRequest)

instance Prelude.Hashable UpdateCsvClassifierRequest where
  hashWithSalt salt' UpdateCsvClassifierRequest' {..} =
    salt' `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` delimiter
      `Prelude.hashWithSalt` allowSingleColumn
      `Prelude.hashWithSalt` header
      `Prelude.hashWithSalt` disableValueTrimming
      `Prelude.hashWithSalt` containsHeader
      `Prelude.hashWithSalt` quoteSymbol

instance Prelude.NFData UpdateCsvClassifierRequest where
  rnf UpdateCsvClassifierRequest' {..} =
    Prelude.rnf quoteSymbol
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf delimiter
      `Prelude.seq` Prelude.rnf allowSingleColumn
      `Prelude.seq` Prelude.rnf header
      `Prelude.seq` Prelude.rnf disableValueTrimming
      `Prelude.seq` Prelude.rnf containsHeader

instance Core.ToJSON UpdateCsvClassifierRequest where
  toJSON UpdateCsvClassifierRequest' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("QuoteSymbol" Core..=) Prelude.<$> quoteSymbol,
            ("ContainsHeader" Core..=)
              Prelude.<$> containsHeader,
            ("DisableValueTrimming" Core..=)
              Prelude.<$> disableValueTrimming,
            ("Header" Core..=) Prelude.<$> header,
            ("AllowSingleColumn" Core..=)
              Prelude.<$> allowSingleColumn,
            ("Delimiter" Core..=) Prelude.<$> delimiter,
            Prelude.Just ("Name" Core..= name)
          ]
      )

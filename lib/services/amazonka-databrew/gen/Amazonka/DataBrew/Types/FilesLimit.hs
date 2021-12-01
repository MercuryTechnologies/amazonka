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
-- Module      : Amazonka.DataBrew.Types.FilesLimit
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DataBrew.Types.FilesLimit where

import qualified Amazonka.Core as Core
import Amazonka.DataBrew.Types.Order
import Amazonka.DataBrew.Types.OrderedBy
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Represents a limit imposed on number of Amazon S3 files that should be
-- selected for a dataset from a connected Amazon S3 path.
--
-- /See:/ 'newFilesLimit' smart constructor.
data FilesLimit = FilesLimit'
  { -- | A criteria to use for Amazon S3 files sorting before their selection. By
    -- default uses LAST_MODIFIED_DATE as a sorting criteria. Currently it\'s
    -- the only allowed value.
    orderedBy :: Prelude.Maybe OrderedBy,
    -- | A criteria to use for Amazon S3 files sorting before their selection. By
    -- default uses DESCENDING order, i.e. most recent files are selected
    -- first. Anotherpossible value is ASCENDING.
    order :: Prelude.Maybe Order,
    -- | The number of Amazon S3 files to select.
    maxFiles :: Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FilesLimit' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'orderedBy', 'filesLimit_orderedBy' - A criteria to use for Amazon S3 files sorting before their selection. By
-- default uses LAST_MODIFIED_DATE as a sorting criteria. Currently it\'s
-- the only allowed value.
--
-- 'order', 'filesLimit_order' - A criteria to use for Amazon S3 files sorting before their selection. By
-- default uses DESCENDING order, i.e. most recent files are selected
-- first. Anotherpossible value is ASCENDING.
--
-- 'maxFiles', 'filesLimit_maxFiles' - The number of Amazon S3 files to select.
newFilesLimit ::
  -- | 'maxFiles'
  Prelude.Natural ->
  FilesLimit
newFilesLimit pMaxFiles_ =
  FilesLimit'
    { orderedBy = Prelude.Nothing,
      order = Prelude.Nothing,
      maxFiles = pMaxFiles_
    }

-- | A criteria to use for Amazon S3 files sorting before their selection. By
-- default uses LAST_MODIFIED_DATE as a sorting criteria. Currently it\'s
-- the only allowed value.
filesLimit_orderedBy :: Lens.Lens' FilesLimit (Prelude.Maybe OrderedBy)
filesLimit_orderedBy = Lens.lens (\FilesLimit' {orderedBy} -> orderedBy) (\s@FilesLimit' {} a -> s {orderedBy = a} :: FilesLimit)

-- | A criteria to use for Amazon S3 files sorting before their selection. By
-- default uses DESCENDING order, i.e. most recent files are selected
-- first. Anotherpossible value is ASCENDING.
filesLimit_order :: Lens.Lens' FilesLimit (Prelude.Maybe Order)
filesLimit_order = Lens.lens (\FilesLimit' {order} -> order) (\s@FilesLimit' {} a -> s {order = a} :: FilesLimit)

-- | The number of Amazon S3 files to select.
filesLimit_maxFiles :: Lens.Lens' FilesLimit Prelude.Natural
filesLimit_maxFiles = Lens.lens (\FilesLimit' {maxFiles} -> maxFiles) (\s@FilesLimit' {} a -> s {maxFiles = a} :: FilesLimit)

instance Core.FromJSON FilesLimit where
  parseJSON =
    Core.withObject
      "FilesLimit"
      ( \x ->
          FilesLimit'
            Prelude.<$> (x Core..:? "OrderedBy")
            Prelude.<*> (x Core..:? "Order")
            Prelude.<*> (x Core..: "MaxFiles")
      )

instance Prelude.Hashable FilesLimit where
  hashWithSalt salt' FilesLimit' {..} =
    salt' `Prelude.hashWithSalt` maxFiles
      `Prelude.hashWithSalt` order
      `Prelude.hashWithSalt` orderedBy

instance Prelude.NFData FilesLimit where
  rnf FilesLimit' {..} =
    Prelude.rnf orderedBy
      `Prelude.seq` Prelude.rnf maxFiles
      `Prelude.seq` Prelude.rnf order

instance Core.ToJSON FilesLimit where
  toJSON FilesLimit' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("OrderedBy" Core..=) Prelude.<$> orderedBy,
            ("Order" Core..=) Prelude.<$> order,
            Prelude.Just ("MaxFiles" Core..= maxFiles)
          ]
      )

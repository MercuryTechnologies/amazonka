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
-- Module      : Amazonka.AppFlow.Types.SourceFieldProperties
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppFlow.Types.SourceFieldProperties where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | The properties that can be applied to a field when the connector is
-- being used as a source.
--
-- /See:/ 'newSourceFieldProperties' smart constructor.
data SourceFieldProperties = SourceFieldProperties'
  { -- | Indicates whether the field can be returned in a search result.
    isRetrievable :: Prelude.Maybe Prelude.Bool,
    -- | Indicates if the field can be queried.
    isQueryable :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SourceFieldProperties' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'isRetrievable', 'sourceFieldProperties_isRetrievable' - Indicates whether the field can be returned in a search result.
--
-- 'isQueryable', 'sourceFieldProperties_isQueryable' - Indicates if the field can be queried.
newSourceFieldProperties ::
  SourceFieldProperties
newSourceFieldProperties =
  SourceFieldProperties'
    { isRetrievable =
        Prelude.Nothing,
      isQueryable = Prelude.Nothing
    }

-- | Indicates whether the field can be returned in a search result.
sourceFieldProperties_isRetrievable :: Lens.Lens' SourceFieldProperties (Prelude.Maybe Prelude.Bool)
sourceFieldProperties_isRetrievable = Lens.lens (\SourceFieldProperties' {isRetrievable} -> isRetrievable) (\s@SourceFieldProperties' {} a -> s {isRetrievable = a} :: SourceFieldProperties)

-- | Indicates if the field can be queried.
sourceFieldProperties_isQueryable :: Lens.Lens' SourceFieldProperties (Prelude.Maybe Prelude.Bool)
sourceFieldProperties_isQueryable = Lens.lens (\SourceFieldProperties' {isQueryable} -> isQueryable) (\s@SourceFieldProperties' {} a -> s {isQueryable = a} :: SourceFieldProperties)

instance Core.FromJSON SourceFieldProperties where
  parseJSON =
    Core.withObject
      "SourceFieldProperties"
      ( \x ->
          SourceFieldProperties'
            Prelude.<$> (x Core..:? "isRetrievable")
            Prelude.<*> (x Core..:? "isQueryable")
      )

instance Prelude.Hashable SourceFieldProperties where
  hashWithSalt salt' SourceFieldProperties' {..} =
    salt' `Prelude.hashWithSalt` isQueryable
      `Prelude.hashWithSalt` isRetrievable

instance Prelude.NFData SourceFieldProperties where
  rnf SourceFieldProperties' {..} =
    Prelude.rnf isRetrievable
      `Prelude.seq` Prelude.rnf isQueryable

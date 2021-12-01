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
-- Module      : Amazonka.IoT.Types.AggregationType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoT.Types.AggregationType where

import qualified Amazonka.Core as Core
import Amazonka.IoT.Types.AggregationTypeName
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | The type of aggregation queries.
--
-- /See:/ 'newAggregationType' smart constructor.
data AggregationType = AggregationType'
  { -- | A list of the values of aggregation types.
    values :: Prelude.Maybe [Prelude.Text],
    -- | The name of the aggregation type.
    name :: AggregationTypeName
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AggregationType' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'values', 'aggregationType_values' - A list of the values of aggregation types.
--
-- 'name', 'aggregationType_name' - The name of the aggregation type.
newAggregationType ::
  -- | 'name'
  AggregationTypeName ->
  AggregationType
newAggregationType pName_ =
  AggregationType'
    { values = Prelude.Nothing,
      name = pName_
    }

-- | A list of the values of aggregation types.
aggregationType_values :: Lens.Lens' AggregationType (Prelude.Maybe [Prelude.Text])
aggregationType_values = Lens.lens (\AggregationType' {values} -> values) (\s@AggregationType' {} a -> s {values = a} :: AggregationType) Prelude.. Lens.mapping Lens.coerced

-- | The name of the aggregation type.
aggregationType_name :: Lens.Lens' AggregationType AggregationTypeName
aggregationType_name = Lens.lens (\AggregationType' {name} -> name) (\s@AggregationType' {} a -> s {name = a} :: AggregationType)

instance Core.FromJSON AggregationType where
  parseJSON =
    Core.withObject
      "AggregationType"
      ( \x ->
          AggregationType'
            Prelude.<$> (x Core..:? "values" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..: "name")
      )

instance Prelude.Hashable AggregationType where
  hashWithSalt salt' AggregationType' {..} =
    salt' `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` values

instance Prelude.NFData AggregationType where
  rnf AggregationType' {..} =
    Prelude.rnf values `Prelude.seq` Prelude.rnf name

instance Core.ToJSON AggregationType where
  toJSON AggregationType' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("values" Core..=) Prelude.<$> values,
            Prelude.Just ("name" Core..= name)
          ]
      )

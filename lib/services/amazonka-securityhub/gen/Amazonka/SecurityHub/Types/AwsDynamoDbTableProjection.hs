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
-- Module      : Amazonka.SecurityHub.Types.AwsDynamoDbTableProjection
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsDynamoDbTableProjection where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | For global and local secondary indexes, identifies the attributes that
-- are copied from the table into the index.
--
-- /See:/ 'newAwsDynamoDbTableProjection' smart constructor.
data AwsDynamoDbTableProjection = AwsDynamoDbTableProjection'
  { -- | The types of attributes that are projected into the index.
    projectionType :: Prelude.Maybe Prelude.Text,
    -- | The nonkey attributes that are projected into the index. For each
    -- attribute, provide the attribute name.
    nonKeyAttributes :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsDynamoDbTableProjection' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'projectionType', 'awsDynamoDbTableProjection_projectionType' - The types of attributes that are projected into the index.
--
-- 'nonKeyAttributes', 'awsDynamoDbTableProjection_nonKeyAttributes' - The nonkey attributes that are projected into the index. For each
-- attribute, provide the attribute name.
newAwsDynamoDbTableProjection ::
  AwsDynamoDbTableProjection
newAwsDynamoDbTableProjection =
  AwsDynamoDbTableProjection'
    { projectionType =
        Prelude.Nothing,
      nonKeyAttributes = Prelude.Nothing
    }

-- | The types of attributes that are projected into the index.
awsDynamoDbTableProjection_projectionType :: Lens.Lens' AwsDynamoDbTableProjection (Prelude.Maybe Prelude.Text)
awsDynamoDbTableProjection_projectionType = Lens.lens (\AwsDynamoDbTableProjection' {projectionType} -> projectionType) (\s@AwsDynamoDbTableProjection' {} a -> s {projectionType = a} :: AwsDynamoDbTableProjection)

-- | The nonkey attributes that are projected into the index. For each
-- attribute, provide the attribute name.
awsDynamoDbTableProjection_nonKeyAttributes :: Lens.Lens' AwsDynamoDbTableProjection (Prelude.Maybe [Prelude.Text])
awsDynamoDbTableProjection_nonKeyAttributes = Lens.lens (\AwsDynamoDbTableProjection' {nonKeyAttributes} -> nonKeyAttributes) (\s@AwsDynamoDbTableProjection' {} a -> s {nonKeyAttributes = a} :: AwsDynamoDbTableProjection) Prelude.. Lens.mapping Lens.coerced

instance Core.FromJSON AwsDynamoDbTableProjection where
  parseJSON =
    Core.withObject
      "AwsDynamoDbTableProjection"
      ( \x ->
          AwsDynamoDbTableProjection'
            Prelude.<$> (x Core..:? "ProjectionType")
            Prelude.<*> ( x Core..:? "NonKeyAttributes"
                            Core..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable AwsDynamoDbTableProjection where
  hashWithSalt salt' AwsDynamoDbTableProjection' {..} =
    salt' `Prelude.hashWithSalt` nonKeyAttributes
      `Prelude.hashWithSalt` projectionType

instance Prelude.NFData AwsDynamoDbTableProjection where
  rnf AwsDynamoDbTableProjection' {..} =
    Prelude.rnf projectionType
      `Prelude.seq` Prelude.rnf nonKeyAttributes

instance Core.ToJSON AwsDynamoDbTableProjection where
  toJSON AwsDynamoDbTableProjection' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("ProjectionType" Core..=)
              Prelude.<$> projectionType,
            ("NonKeyAttributes" Core..=)
              Prelude.<$> nonKeyAttributes
          ]
      )

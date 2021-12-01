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
-- Module      : Amazonka.SecurityHub.Types.AwsDynamoDbTableStreamSpecification
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsDynamoDbTableStreamSpecification where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | The current DynamoDB Streams configuration for the table.
--
-- /See:/ 'newAwsDynamoDbTableStreamSpecification' smart constructor.
data AwsDynamoDbTableStreamSpecification = AwsDynamoDbTableStreamSpecification'
  { -- | Determines the information that is written to the table.
    streamViewType :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether DynamoDB Streams is enabled on the table.
    streamEnabled :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsDynamoDbTableStreamSpecification' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'streamViewType', 'awsDynamoDbTableStreamSpecification_streamViewType' - Determines the information that is written to the table.
--
-- 'streamEnabled', 'awsDynamoDbTableStreamSpecification_streamEnabled' - Indicates whether DynamoDB Streams is enabled on the table.
newAwsDynamoDbTableStreamSpecification ::
  AwsDynamoDbTableStreamSpecification
newAwsDynamoDbTableStreamSpecification =
  AwsDynamoDbTableStreamSpecification'
    { streamViewType =
        Prelude.Nothing,
      streamEnabled = Prelude.Nothing
    }

-- | Determines the information that is written to the table.
awsDynamoDbTableStreamSpecification_streamViewType :: Lens.Lens' AwsDynamoDbTableStreamSpecification (Prelude.Maybe Prelude.Text)
awsDynamoDbTableStreamSpecification_streamViewType = Lens.lens (\AwsDynamoDbTableStreamSpecification' {streamViewType} -> streamViewType) (\s@AwsDynamoDbTableStreamSpecification' {} a -> s {streamViewType = a} :: AwsDynamoDbTableStreamSpecification)

-- | Indicates whether DynamoDB Streams is enabled on the table.
awsDynamoDbTableStreamSpecification_streamEnabled :: Lens.Lens' AwsDynamoDbTableStreamSpecification (Prelude.Maybe Prelude.Bool)
awsDynamoDbTableStreamSpecification_streamEnabled = Lens.lens (\AwsDynamoDbTableStreamSpecification' {streamEnabled} -> streamEnabled) (\s@AwsDynamoDbTableStreamSpecification' {} a -> s {streamEnabled = a} :: AwsDynamoDbTableStreamSpecification)

instance
  Core.FromJSON
    AwsDynamoDbTableStreamSpecification
  where
  parseJSON =
    Core.withObject
      "AwsDynamoDbTableStreamSpecification"
      ( \x ->
          AwsDynamoDbTableStreamSpecification'
            Prelude.<$> (x Core..:? "StreamViewType")
            Prelude.<*> (x Core..:? "StreamEnabled")
      )

instance
  Prelude.Hashable
    AwsDynamoDbTableStreamSpecification
  where
  hashWithSalt
    salt'
    AwsDynamoDbTableStreamSpecification' {..} =
      salt' `Prelude.hashWithSalt` streamEnabled
        `Prelude.hashWithSalt` streamViewType

instance
  Prelude.NFData
    AwsDynamoDbTableStreamSpecification
  where
  rnf AwsDynamoDbTableStreamSpecification' {..} =
    Prelude.rnf streamViewType
      `Prelude.seq` Prelude.rnf streamEnabled

instance
  Core.ToJSON
    AwsDynamoDbTableStreamSpecification
  where
  toJSON AwsDynamoDbTableStreamSpecification' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("StreamViewType" Core..=)
              Prelude.<$> streamViewType,
            ("StreamEnabled" Core..=) Prelude.<$> streamEnabled
          ]
      )

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
-- Module      : Amazonka.SecurityHub.Types.AwsEcsTaskDefinitionContainerDefinitionsDependsOnDetails
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsEcsTaskDefinitionContainerDefinitionsDependsOnDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | A dependency that is defined for container startup and shutdown.
--
-- /See:/ 'newAwsEcsTaskDefinitionContainerDefinitionsDependsOnDetails' smart constructor.
data AwsEcsTaskDefinitionContainerDefinitionsDependsOnDetails = AwsEcsTaskDefinitionContainerDefinitionsDependsOnDetails'
  { -- | The name of the dependent container.
    containerName :: Prelude.Maybe Prelude.Text,
    -- | The dependency condition of the dependent container. Indicates the
    -- required status of the dependent container before the current container
    -- can start.
    condition :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsEcsTaskDefinitionContainerDefinitionsDependsOnDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'containerName', 'awsEcsTaskDefinitionContainerDefinitionsDependsOnDetails_containerName' - The name of the dependent container.
--
-- 'condition', 'awsEcsTaskDefinitionContainerDefinitionsDependsOnDetails_condition' - The dependency condition of the dependent container. Indicates the
-- required status of the dependent container before the current container
-- can start.
newAwsEcsTaskDefinitionContainerDefinitionsDependsOnDetails ::
  AwsEcsTaskDefinitionContainerDefinitionsDependsOnDetails
newAwsEcsTaskDefinitionContainerDefinitionsDependsOnDetails =
  AwsEcsTaskDefinitionContainerDefinitionsDependsOnDetails'
    { containerName =
        Prelude.Nothing,
      condition =
        Prelude.Nothing
    }

-- | The name of the dependent container.
awsEcsTaskDefinitionContainerDefinitionsDependsOnDetails_containerName :: Lens.Lens' AwsEcsTaskDefinitionContainerDefinitionsDependsOnDetails (Prelude.Maybe Prelude.Text)
awsEcsTaskDefinitionContainerDefinitionsDependsOnDetails_containerName = Lens.lens (\AwsEcsTaskDefinitionContainerDefinitionsDependsOnDetails' {containerName} -> containerName) (\s@AwsEcsTaskDefinitionContainerDefinitionsDependsOnDetails' {} a -> s {containerName = a} :: AwsEcsTaskDefinitionContainerDefinitionsDependsOnDetails)

-- | The dependency condition of the dependent container. Indicates the
-- required status of the dependent container before the current container
-- can start.
awsEcsTaskDefinitionContainerDefinitionsDependsOnDetails_condition :: Lens.Lens' AwsEcsTaskDefinitionContainerDefinitionsDependsOnDetails (Prelude.Maybe Prelude.Text)
awsEcsTaskDefinitionContainerDefinitionsDependsOnDetails_condition = Lens.lens (\AwsEcsTaskDefinitionContainerDefinitionsDependsOnDetails' {condition} -> condition) (\s@AwsEcsTaskDefinitionContainerDefinitionsDependsOnDetails' {} a -> s {condition = a} :: AwsEcsTaskDefinitionContainerDefinitionsDependsOnDetails)

instance
  Core.FromJSON
    AwsEcsTaskDefinitionContainerDefinitionsDependsOnDetails
  where
  parseJSON =
    Core.withObject
      "AwsEcsTaskDefinitionContainerDefinitionsDependsOnDetails"
      ( \x ->
          AwsEcsTaskDefinitionContainerDefinitionsDependsOnDetails'
            Prelude.<$> (x Core..:? "ContainerName")
              Prelude.<*> (x Core..:? "Condition")
      )

instance
  Prelude.Hashable
    AwsEcsTaskDefinitionContainerDefinitionsDependsOnDetails
  where
  hashWithSalt
    salt'
    AwsEcsTaskDefinitionContainerDefinitionsDependsOnDetails' {..} =
      salt' `Prelude.hashWithSalt` condition
        `Prelude.hashWithSalt` containerName

instance
  Prelude.NFData
    AwsEcsTaskDefinitionContainerDefinitionsDependsOnDetails
  where
  rnf
    AwsEcsTaskDefinitionContainerDefinitionsDependsOnDetails' {..} =
      Prelude.rnf containerName
        `Prelude.seq` Prelude.rnf condition

instance
  Core.ToJSON
    AwsEcsTaskDefinitionContainerDefinitionsDependsOnDetails
  where
  toJSON
    AwsEcsTaskDefinitionContainerDefinitionsDependsOnDetails' {..} =
      Core.object
        ( Prelude.catMaybes
            [ ("ContainerName" Core..=) Prelude.<$> containerName,
              ("Condition" Core..=) Prelude.<$> condition
            ]
        )

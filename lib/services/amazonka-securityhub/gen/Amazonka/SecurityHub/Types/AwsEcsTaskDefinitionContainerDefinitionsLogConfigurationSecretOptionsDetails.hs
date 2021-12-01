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
-- Module      : Amazonka.SecurityHub.Types.AwsEcsTaskDefinitionContainerDefinitionsLogConfigurationSecretOptionsDetails
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsEcsTaskDefinitionContainerDefinitionsLogConfigurationSecretOptionsDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | A secret to pass to the log configuration.
--
-- /See:/ 'newAwsEcsTaskDefinitionContainerDefinitionsLogConfigurationSecretOptionsDetails' smart constructor.
data AwsEcsTaskDefinitionContainerDefinitionsLogConfigurationSecretOptionsDetails = AwsEcsTaskDefinitionContainerDefinitionsLogConfigurationSecretOptionsDetails'
  { -- | The name of the secret.
    name :: Prelude.Maybe Prelude.Text,
    -- | The secret to expose to the container.
    --
    -- The value is either the full ARN of the Secrets Manager secret or the
    -- full ARN of the parameter in the Systems Manager Parameter Store.
    valueFrom :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsEcsTaskDefinitionContainerDefinitionsLogConfigurationSecretOptionsDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'awsEcsTaskDefinitionContainerDefinitionsLogConfigurationSecretOptionsDetails_name' - The name of the secret.
--
-- 'valueFrom', 'awsEcsTaskDefinitionContainerDefinitionsLogConfigurationSecretOptionsDetails_valueFrom' - The secret to expose to the container.
--
-- The value is either the full ARN of the Secrets Manager secret or the
-- full ARN of the parameter in the Systems Manager Parameter Store.
newAwsEcsTaskDefinitionContainerDefinitionsLogConfigurationSecretOptionsDetails ::
  AwsEcsTaskDefinitionContainerDefinitionsLogConfigurationSecretOptionsDetails
newAwsEcsTaskDefinitionContainerDefinitionsLogConfigurationSecretOptionsDetails =
  AwsEcsTaskDefinitionContainerDefinitionsLogConfigurationSecretOptionsDetails'
    { name =
        Prelude.Nothing,
      valueFrom =
        Prelude.Nothing
    }

-- | The name of the secret.
awsEcsTaskDefinitionContainerDefinitionsLogConfigurationSecretOptionsDetails_name :: Lens.Lens' AwsEcsTaskDefinitionContainerDefinitionsLogConfigurationSecretOptionsDetails (Prelude.Maybe Prelude.Text)
awsEcsTaskDefinitionContainerDefinitionsLogConfigurationSecretOptionsDetails_name = Lens.lens (\AwsEcsTaskDefinitionContainerDefinitionsLogConfigurationSecretOptionsDetails' {name} -> name) (\s@AwsEcsTaskDefinitionContainerDefinitionsLogConfigurationSecretOptionsDetails' {} a -> s {name = a} :: AwsEcsTaskDefinitionContainerDefinitionsLogConfigurationSecretOptionsDetails)

-- | The secret to expose to the container.
--
-- The value is either the full ARN of the Secrets Manager secret or the
-- full ARN of the parameter in the Systems Manager Parameter Store.
awsEcsTaskDefinitionContainerDefinitionsLogConfigurationSecretOptionsDetails_valueFrom :: Lens.Lens' AwsEcsTaskDefinitionContainerDefinitionsLogConfigurationSecretOptionsDetails (Prelude.Maybe Prelude.Text)
awsEcsTaskDefinitionContainerDefinitionsLogConfigurationSecretOptionsDetails_valueFrom = Lens.lens (\AwsEcsTaskDefinitionContainerDefinitionsLogConfigurationSecretOptionsDetails' {valueFrom} -> valueFrom) (\s@AwsEcsTaskDefinitionContainerDefinitionsLogConfigurationSecretOptionsDetails' {} a -> s {valueFrom = a} :: AwsEcsTaskDefinitionContainerDefinitionsLogConfigurationSecretOptionsDetails)

instance
  Core.FromJSON
    AwsEcsTaskDefinitionContainerDefinitionsLogConfigurationSecretOptionsDetails
  where
  parseJSON =
    Core.withObject
      "AwsEcsTaskDefinitionContainerDefinitionsLogConfigurationSecretOptionsDetails"
      ( \x ->
          AwsEcsTaskDefinitionContainerDefinitionsLogConfigurationSecretOptionsDetails'
            Prelude.<$> (x Core..:? "Name")
              Prelude.<*> (x Core..:? "ValueFrom")
      )

instance
  Prelude.Hashable
    AwsEcsTaskDefinitionContainerDefinitionsLogConfigurationSecretOptionsDetails
  where
  hashWithSalt
    salt'
    AwsEcsTaskDefinitionContainerDefinitionsLogConfigurationSecretOptionsDetails' {..} =
      salt' `Prelude.hashWithSalt` valueFrom
        `Prelude.hashWithSalt` name

instance
  Prelude.NFData
    AwsEcsTaskDefinitionContainerDefinitionsLogConfigurationSecretOptionsDetails
  where
  rnf
    AwsEcsTaskDefinitionContainerDefinitionsLogConfigurationSecretOptionsDetails' {..} =
      Prelude.rnf name
        `Prelude.seq` Prelude.rnf valueFrom

instance
  Core.ToJSON
    AwsEcsTaskDefinitionContainerDefinitionsLogConfigurationSecretOptionsDetails
  where
  toJSON
    AwsEcsTaskDefinitionContainerDefinitionsLogConfigurationSecretOptionsDetails' {..} =
      Core.object
        ( Prelude.catMaybes
            [ ("Name" Core..=) Prelude.<$> name,
              ("ValueFrom" Core..=) Prelude.<$> valueFrom
            ]
        )

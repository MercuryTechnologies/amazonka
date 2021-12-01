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
-- Module      : Amazonka.SecurityHub.Types.AwsEcrRepositoryLifecyclePolicyDetails
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsEcrRepositoryLifecyclePolicyDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Information about the lifecycle policy for the repository.
--
-- /See:/ 'newAwsEcrRepositoryLifecyclePolicyDetails' smart constructor.
data AwsEcrRepositoryLifecyclePolicyDetails = AwsEcrRepositoryLifecyclePolicyDetails'
  { -- | The Amazon Web Services account identifier that is associated with the
    -- registry that contains the repository.
    registryId :: Prelude.Maybe Prelude.Text,
    -- | The text of the lifecycle policy.
    lifecyclePolicyText :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsEcrRepositoryLifecyclePolicyDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'registryId', 'awsEcrRepositoryLifecyclePolicyDetails_registryId' - The Amazon Web Services account identifier that is associated with the
-- registry that contains the repository.
--
-- 'lifecyclePolicyText', 'awsEcrRepositoryLifecyclePolicyDetails_lifecyclePolicyText' - The text of the lifecycle policy.
newAwsEcrRepositoryLifecyclePolicyDetails ::
  AwsEcrRepositoryLifecyclePolicyDetails
newAwsEcrRepositoryLifecyclePolicyDetails =
  AwsEcrRepositoryLifecyclePolicyDetails'
    { registryId =
        Prelude.Nothing,
      lifecyclePolicyText =
        Prelude.Nothing
    }

-- | The Amazon Web Services account identifier that is associated with the
-- registry that contains the repository.
awsEcrRepositoryLifecyclePolicyDetails_registryId :: Lens.Lens' AwsEcrRepositoryLifecyclePolicyDetails (Prelude.Maybe Prelude.Text)
awsEcrRepositoryLifecyclePolicyDetails_registryId = Lens.lens (\AwsEcrRepositoryLifecyclePolicyDetails' {registryId} -> registryId) (\s@AwsEcrRepositoryLifecyclePolicyDetails' {} a -> s {registryId = a} :: AwsEcrRepositoryLifecyclePolicyDetails)

-- | The text of the lifecycle policy.
awsEcrRepositoryLifecyclePolicyDetails_lifecyclePolicyText :: Lens.Lens' AwsEcrRepositoryLifecyclePolicyDetails (Prelude.Maybe Prelude.Text)
awsEcrRepositoryLifecyclePolicyDetails_lifecyclePolicyText = Lens.lens (\AwsEcrRepositoryLifecyclePolicyDetails' {lifecyclePolicyText} -> lifecyclePolicyText) (\s@AwsEcrRepositoryLifecyclePolicyDetails' {} a -> s {lifecyclePolicyText = a} :: AwsEcrRepositoryLifecyclePolicyDetails)

instance
  Core.FromJSON
    AwsEcrRepositoryLifecyclePolicyDetails
  where
  parseJSON =
    Core.withObject
      "AwsEcrRepositoryLifecyclePolicyDetails"
      ( \x ->
          AwsEcrRepositoryLifecyclePolicyDetails'
            Prelude.<$> (x Core..:? "RegistryId")
            Prelude.<*> (x Core..:? "LifecyclePolicyText")
      )

instance
  Prelude.Hashable
    AwsEcrRepositoryLifecyclePolicyDetails
  where
  hashWithSalt
    salt'
    AwsEcrRepositoryLifecyclePolicyDetails' {..} =
      salt' `Prelude.hashWithSalt` lifecyclePolicyText
        `Prelude.hashWithSalt` registryId

instance
  Prelude.NFData
    AwsEcrRepositoryLifecyclePolicyDetails
  where
  rnf AwsEcrRepositoryLifecyclePolicyDetails' {..} =
    Prelude.rnf registryId
      `Prelude.seq` Prelude.rnf lifecyclePolicyText

instance
  Core.ToJSON
    AwsEcrRepositoryLifecyclePolicyDetails
  where
  toJSON AwsEcrRepositoryLifecyclePolicyDetails' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("RegistryId" Core..=) Prelude.<$> registryId,
            ("LifecyclePolicyText" Core..=)
              Prelude.<$> lifecyclePolicyText
          ]
      )

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
-- Module      : Amazonka.ImageBuilder.Types.InfrastructureConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ImageBuilder.Types.InfrastructureConfiguration where

import qualified Amazonka.Core as Core
import Amazonka.ImageBuilder.Types.InstanceMetadataOptions
import Amazonka.ImageBuilder.Types.Logging
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Details of the infrastructure configuration.
--
-- /See:/ 'newInfrastructureConfiguration' smart constructor.
data InfrastructureConfiguration = InfrastructureConfiguration'
  { -- | The security group IDs of the infrastructure configuration.
    securityGroupIds :: Prelude.Maybe [Prelude.Text],
    -- | The SNS topic Amazon Resource Name (ARN) of the infrastructure
    -- configuration.
    snsTopicArn :: Prelude.Maybe Prelude.Text,
    -- | The instance types of the infrastructure configuration.
    instanceTypes :: Prelude.Maybe [Prelude.Text],
    -- | The Amazon Resource Name (ARN) of the infrastructure configuration.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The Amazon EC2 key pair of the infrastructure configuration.
    keyPair :: Prelude.Maybe Prelude.Text,
    -- | The tags attached to the resource created by Image Builder.
    resourceTags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The subnet ID of the infrastructure configuration.
    subnetId :: Prelude.Maybe Prelude.Text,
    -- | The date on which the infrastructure configuration was last updated.
    dateUpdated :: Prelude.Maybe Prelude.Text,
    -- | The date on which the infrastructure configuration was created.
    dateCreated :: Prelude.Maybe Prelude.Text,
    -- | The instance metadata option settings for the infrastructure
    -- configuration.
    instanceMetadataOptions :: Prelude.Maybe InstanceMetadataOptions,
    -- | The name of the infrastructure configuration.
    name :: Prelude.Maybe Prelude.Text,
    -- | The instance profile of the infrastructure configuration.
    instanceProfileName :: Prelude.Maybe Prelude.Text,
    -- | The logging configuration of the infrastructure configuration.
    logging :: Prelude.Maybe Logging,
    -- | The description of the infrastructure configuration.
    description :: Prelude.Maybe Prelude.Text,
    -- | The tags of the infrastructure configuration.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The terminate instance on failure configuration of the infrastructure
    -- configuration.
    terminateInstanceOnFailure :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InfrastructureConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'securityGroupIds', 'infrastructureConfiguration_securityGroupIds' - The security group IDs of the infrastructure configuration.
--
-- 'snsTopicArn', 'infrastructureConfiguration_snsTopicArn' - The SNS topic Amazon Resource Name (ARN) of the infrastructure
-- configuration.
--
-- 'instanceTypes', 'infrastructureConfiguration_instanceTypes' - The instance types of the infrastructure configuration.
--
-- 'arn', 'infrastructureConfiguration_arn' - The Amazon Resource Name (ARN) of the infrastructure configuration.
--
-- 'keyPair', 'infrastructureConfiguration_keyPair' - The Amazon EC2 key pair of the infrastructure configuration.
--
-- 'resourceTags', 'infrastructureConfiguration_resourceTags' - The tags attached to the resource created by Image Builder.
--
-- 'subnetId', 'infrastructureConfiguration_subnetId' - The subnet ID of the infrastructure configuration.
--
-- 'dateUpdated', 'infrastructureConfiguration_dateUpdated' - The date on which the infrastructure configuration was last updated.
--
-- 'dateCreated', 'infrastructureConfiguration_dateCreated' - The date on which the infrastructure configuration was created.
--
-- 'instanceMetadataOptions', 'infrastructureConfiguration_instanceMetadataOptions' - The instance metadata option settings for the infrastructure
-- configuration.
--
-- 'name', 'infrastructureConfiguration_name' - The name of the infrastructure configuration.
--
-- 'instanceProfileName', 'infrastructureConfiguration_instanceProfileName' - The instance profile of the infrastructure configuration.
--
-- 'logging', 'infrastructureConfiguration_logging' - The logging configuration of the infrastructure configuration.
--
-- 'description', 'infrastructureConfiguration_description' - The description of the infrastructure configuration.
--
-- 'tags', 'infrastructureConfiguration_tags' - The tags of the infrastructure configuration.
--
-- 'terminateInstanceOnFailure', 'infrastructureConfiguration_terminateInstanceOnFailure' - The terminate instance on failure configuration of the infrastructure
-- configuration.
newInfrastructureConfiguration ::
  InfrastructureConfiguration
newInfrastructureConfiguration =
  InfrastructureConfiguration'
    { securityGroupIds =
        Prelude.Nothing,
      snsTopicArn = Prelude.Nothing,
      instanceTypes = Prelude.Nothing,
      arn = Prelude.Nothing,
      keyPair = Prelude.Nothing,
      resourceTags = Prelude.Nothing,
      subnetId = Prelude.Nothing,
      dateUpdated = Prelude.Nothing,
      dateCreated = Prelude.Nothing,
      instanceMetadataOptions = Prelude.Nothing,
      name = Prelude.Nothing,
      instanceProfileName = Prelude.Nothing,
      logging = Prelude.Nothing,
      description = Prelude.Nothing,
      tags = Prelude.Nothing,
      terminateInstanceOnFailure = Prelude.Nothing
    }

-- | The security group IDs of the infrastructure configuration.
infrastructureConfiguration_securityGroupIds :: Lens.Lens' InfrastructureConfiguration (Prelude.Maybe [Prelude.Text])
infrastructureConfiguration_securityGroupIds = Lens.lens (\InfrastructureConfiguration' {securityGroupIds} -> securityGroupIds) (\s@InfrastructureConfiguration' {} a -> s {securityGroupIds = a} :: InfrastructureConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | The SNS topic Amazon Resource Name (ARN) of the infrastructure
-- configuration.
infrastructureConfiguration_snsTopicArn :: Lens.Lens' InfrastructureConfiguration (Prelude.Maybe Prelude.Text)
infrastructureConfiguration_snsTopicArn = Lens.lens (\InfrastructureConfiguration' {snsTopicArn} -> snsTopicArn) (\s@InfrastructureConfiguration' {} a -> s {snsTopicArn = a} :: InfrastructureConfiguration)

-- | The instance types of the infrastructure configuration.
infrastructureConfiguration_instanceTypes :: Lens.Lens' InfrastructureConfiguration (Prelude.Maybe [Prelude.Text])
infrastructureConfiguration_instanceTypes = Lens.lens (\InfrastructureConfiguration' {instanceTypes} -> instanceTypes) (\s@InfrastructureConfiguration' {} a -> s {instanceTypes = a} :: InfrastructureConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Resource Name (ARN) of the infrastructure configuration.
infrastructureConfiguration_arn :: Lens.Lens' InfrastructureConfiguration (Prelude.Maybe Prelude.Text)
infrastructureConfiguration_arn = Lens.lens (\InfrastructureConfiguration' {arn} -> arn) (\s@InfrastructureConfiguration' {} a -> s {arn = a} :: InfrastructureConfiguration)

-- | The Amazon EC2 key pair of the infrastructure configuration.
infrastructureConfiguration_keyPair :: Lens.Lens' InfrastructureConfiguration (Prelude.Maybe Prelude.Text)
infrastructureConfiguration_keyPair = Lens.lens (\InfrastructureConfiguration' {keyPair} -> keyPair) (\s@InfrastructureConfiguration' {} a -> s {keyPair = a} :: InfrastructureConfiguration)

-- | The tags attached to the resource created by Image Builder.
infrastructureConfiguration_resourceTags :: Lens.Lens' InfrastructureConfiguration (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
infrastructureConfiguration_resourceTags = Lens.lens (\InfrastructureConfiguration' {resourceTags} -> resourceTags) (\s@InfrastructureConfiguration' {} a -> s {resourceTags = a} :: InfrastructureConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | The subnet ID of the infrastructure configuration.
infrastructureConfiguration_subnetId :: Lens.Lens' InfrastructureConfiguration (Prelude.Maybe Prelude.Text)
infrastructureConfiguration_subnetId = Lens.lens (\InfrastructureConfiguration' {subnetId} -> subnetId) (\s@InfrastructureConfiguration' {} a -> s {subnetId = a} :: InfrastructureConfiguration)

-- | The date on which the infrastructure configuration was last updated.
infrastructureConfiguration_dateUpdated :: Lens.Lens' InfrastructureConfiguration (Prelude.Maybe Prelude.Text)
infrastructureConfiguration_dateUpdated = Lens.lens (\InfrastructureConfiguration' {dateUpdated} -> dateUpdated) (\s@InfrastructureConfiguration' {} a -> s {dateUpdated = a} :: InfrastructureConfiguration)

-- | The date on which the infrastructure configuration was created.
infrastructureConfiguration_dateCreated :: Lens.Lens' InfrastructureConfiguration (Prelude.Maybe Prelude.Text)
infrastructureConfiguration_dateCreated = Lens.lens (\InfrastructureConfiguration' {dateCreated} -> dateCreated) (\s@InfrastructureConfiguration' {} a -> s {dateCreated = a} :: InfrastructureConfiguration)

-- | The instance metadata option settings for the infrastructure
-- configuration.
infrastructureConfiguration_instanceMetadataOptions :: Lens.Lens' InfrastructureConfiguration (Prelude.Maybe InstanceMetadataOptions)
infrastructureConfiguration_instanceMetadataOptions = Lens.lens (\InfrastructureConfiguration' {instanceMetadataOptions} -> instanceMetadataOptions) (\s@InfrastructureConfiguration' {} a -> s {instanceMetadataOptions = a} :: InfrastructureConfiguration)

-- | The name of the infrastructure configuration.
infrastructureConfiguration_name :: Lens.Lens' InfrastructureConfiguration (Prelude.Maybe Prelude.Text)
infrastructureConfiguration_name = Lens.lens (\InfrastructureConfiguration' {name} -> name) (\s@InfrastructureConfiguration' {} a -> s {name = a} :: InfrastructureConfiguration)

-- | The instance profile of the infrastructure configuration.
infrastructureConfiguration_instanceProfileName :: Lens.Lens' InfrastructureConfiguration (Prelude.Maybe Prelude.Text)
infrastructureConfiguration_instanceProfileName = Lens.lens (\InfrastructureConfiguration' {instanceProfileName} -> instanceProfileName) (\s@InfrastructureConfiguration' {} a -> s {instanceProfileName = a} :: InfrastructureConfiguration)

-- | The logging configuration of the infrastructure configuration.
infrastructureConfiguration_logging :: Lens.Lens' InfrastructureConfiguration (Prelude.Maybe Logging)
infrastructureConfiguration_logging = Lens.lens (\InfrastructureConfiguration' {logging} -> logging) (\s@InfrastructureConfiguration' {} a -> s {logging = a} :: InfrastructureConfiguration)

-- | The description of the infrastructure configuration.
infrastructureConfiguration_description :: Lens.Lens' InfrastructureConfiguration (Prelude.Maybe Prelude.Text)
infrastructureConfiguration_description = Lens.lens (\InfrastructureConfiguration' {description} -> description) (\s@InfrastructureConfiguration' {} a -> s {description = a} :: InfrastructureConfiguration)

-- | The tags of the infrastructure configuration.
infrastructureConfiguration_tags :: Lens.Lens' InfrastructureConfiguration (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
infrastructureConfiguration_tags = Lens.lens (\InfrastructureConfiguration' {tags} -> tags) (\s@InfrastructureConfiguration' {} a -> s {tags = a} :: InfrastructureConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | The terminate instance on failure configuration of the infrastructure
-- configuration.
infrastructureConfiguration_terminateInstanceOnFailure :: Lens.Lens' InfrastructureConfiguration (Prelude.Maybe Prelude.Bool)
infrastructureConfiguration_terminateInstanceOnFailure = Lens.lens (\InfrastructureConfiguration' {terminateInstanceOnFailure} -> terminateInstanceOnFailure) (\s@InfrastructureConfiguration' {} a -> s {terminateInstanceOnFailure = a} :: InfrastructureConfiguration)

instance Core.FromJSON InfrastructureConfiguration where
  parseJSON =
    Core.withObject
      "InfrastructureConfiguration"
      ( \x ->
          InfrastructureConfiguration'
            Prelude.<$> ( x Core..:? "securityGroupIds"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "snsTopicArn")
            Prelude.<*> (x Core..:? "instanceTypes" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "arn")
            Prelude.<*> (x Core..:? "keyPair")
            Prelude.<*> (x Core..:? "resourceTags" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "subnetId")
            Prelude.<*> (x Core..:? "dateUpdated")
            Prelude.<*> (x Core..:? "dateCreated")
            Prelude.<*> (x Core..:? "instanceMetadataOptions")
            Prelude.<*> (x Core..:? "name")
            Prelude.<*> (x Core..:? "instanceProfileName")
            Prelude.<*> (x Core..:? "logging")
            Prelude.<*> (x Core..:? "description")
            Prelude.<*> (x Core..:? "tags" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "terminateInstanceOnFailure")
      )

instance Prelude.Hashable InfrastructureConfiguration where
  hashWithSalt salt' InfrastructureConfiguration' {..} =
    salt'
      `Prelude.hashWithSalt` terminateInstanceOnFailure
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` logging
      `Prelude.hashWithSalt` instanceProfileName
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` instanceMetadataOptions
      `Prelude.hashWithSalt` dateCreated
      `Prelude.hashWithSalt` dateUpdated
      `Prelude.hashWithSalt` subnetId
      `Prelude.hashWithSalt` resourceTags
      `Prelude.hashWithSalt` keyPair
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` instanceTypes
      `Prelude.hashWithSalt` snsTopicArn
      `Prelude.hashWithSalt` securityGroupIds

instance Prelude.NFData InfrastructureConfiguration where
  rnf InfrastructureConfiguration' {..} =
    Prelude.rnf securityGroupIds
      `Prelude.seq` Prelude.rnf terminateInstanceOnFailure
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf logging
      `Prelude.seq` Prelude.rnf instanceProfileName
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf instanceMetadataOptions
      `Prelude.seq` Prelude.rnf dateCreated
      `Prelude.seq` Prelude.rnf dateUpdated
      `Prelude.seq` Prelude.rnf subnetId
      `Prelude.seq` Prelude.rnf resourceTags
      `Prelude.seq` Prelude.rnf keyPair
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf instanceTypes
      `Prelude.seq` Prelude.rnf snsTopicArn

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
-- Module      : Amazonka.Connect.Types.RoutingProfile
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Connect.Types.RoutingProfile where

import Amazonka.Connect.Types.MediaConcurrency
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Contains information about a routing profile.
--
-- /See:/ 'newRoutingProfile' smart constructor.
data RoutingProfile = RoutingProfile'
  { -- | The identifier of the Amazon Connect instance. You can find the
    -- instanceId in the ARN of the instance.
    instanceId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the routing profile.
    routingProfileArn :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the routing profile.
    routingProfileId :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the default outbound queue for this routing profile.
    defaultOutboundQueueId :: Prelude.Maybe Prelude.Text,
    -- | The name of the routing profile.
    name :: Prelude.Maybe Prelude.Text,
    -- | The channels agents can handle in the Contact Control Panel (CCP) for
    -- this routing profile.
    mediaConcurrencies :: Prelude.Maybe [MediaConcurrency],
    -- | The description of the routing profile.
    description :: Prelude.Maybe Prelude.Text,
    -- | One or more tags.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RoutingProfile' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceId', 'routingProfile_instanceId' - The identifier of the Amazon Connect instance. You can find the
-- instanceId in the ARN of the instance.
--
-- 'routingProfileArn', 'routingProfile_routingProfileArn' - The Amazon Resource Name (ARN) of the routing profile.
--
-- 'routingProfileId', 'routingProfile_routingProfileId' - The identifier of the routing profile.
--
-- 'defaultOutboundQueueId', 'routingProfile_defaultOutboundQueueId' - The identifier of the default outbound queue for this routing profile.
--
-- 'name', 'routingProfile_name' - The name of the routing profile.
--
-- 'mediaConcurrencies', 'routingProfile_mediaConcurrencies' - The channels agents can handle in the Contact Control Panel (CCP) for
-- this routing profile.
--
-- 'description', 'routingProfile_description' - The description of the routing profile.
--
-- 'tags', 'routingProfile_tags' - One or more tags.
newRoutingProfile ::
  RoutingProfile
newRoutingProfile =
  RoutingProfile'
    { instanceId = Prelude.Nothing,
      routingProfileArn = Prelude.Nothing,
      routingProfileId = Prelude.Nothing,
      defaultOutboundQueueId = Prelude.Nothing,
      name = Prelude.Nothing,
      mediaConcurrencies = Prelude.Nothing,
      description = Prelude.Nothing,
      tags = Prelude.Nothing
    }

-- | The identifier of the Amazon Connect instance. You can find the
-- instanceId in the ARN of the instance.
routingProfile_instanceId :: Lens.Lens' RoutingProfile (Prelude.Maybe Prelude.Text)
routingProfile_instanceId = Lens.lens (\RoutingProfile' {instanceId} -> instanceId) (\s@RoutingProfile' {} a -> s {instanceId = a} :: RoutingProfile)

-- | The Amazon Resource Name (ARN) of the routing profile.
routingProfile_routingProfileArn :: Lens.Lens' RoutingProfile (Prelude.Maybe Prelude.Text)
routingProfile_routingProfileArn = Lens.lens (\RoutingProfile' {routingProfileArn} -> routingProfileArn) (\s@RoutingProfile' {} a -> s {routingProfileArn = a} :: RoutingProfile)

-- | The identifier of the routing profile.
routingProfile_routingProfileId :: Lens.Lens' RoutingProfile (Prelude.Maybe Prelude.Text)
routingProfile_routingProfileId = Lens.lens (\RoutingProfile' {routingProfileId} -> routingProfileId) (\s@RoutingProfile' {} a -> s {routingProfileId = a} :: RoutingProfile)

-- | The identifier of the default outbound queue for this routing profile.
routingProfile_defaultOutboundQueueId :: Lens.Lens' RoutingProfile (Prelude.Maybe Prelude.Text)
routingProfile_defaultOutboundQueueId = Lens.lens (\RoutingProfile' {defaultOutboundQueueId} -> defaultOutboundQueueId) (\s@RoutingProfile' {} a -> s {defaultOutboundQueueId = a} :: RoutingProfile)

-- | The name of the routing profile.
routingProfile_name :: Lens.Lens' RoutingProfile (Prelude.Maybe Prelude.Text)
routingProfile_name = Lens.lens (\RoutingProfile' {name} -> name) (\s@RoutingProfile' {} a -> s {name = a} :: RoutingProfile)

-- | The channels agents can handle in the Contact Control Panel (CCP) for
-- this routing profile.
routingProfile_mediaConcurrencies :: Lens.Lens' RoutingProfile (Prelude.Maybe [MediaConcurrency])
routingProfile_mediaConcurrencies = Lens.lens (\RoutingProfile' {mediaConcurrencies} -> mediaConcurrencies) (\s@RoutingProfile' {} a -> s {mediaConcurrencies = a} :: RoutingProfile) Prelude.. Lens.mapping Lens.coerced

-- | The description of the routing profile.
routingProfile_description :: Lens.Lens' RoutingProfile (Prelude.Maybe Prelude.Text)
routingProfile_description = Lens.lens (\RoutingProfile' {description} -> description) (\s@RoutingProfile' {} a -> s {description = a} :: RoutingProfile)

-- | One or more tags.
routingProfile_tags :: Lens.Lens' RoutingProfile (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
routingProfile_tags = Lens.lens (\RoutingProfile' {tags} -> tags) (\s@RoutingProfile' {} a -> s {tags = a} :: RoutingProfile) Prelude.. Lens.mapping Lens.coerced

instance Core.FromJSON RoutingProfile where
  parseJSON =
    Core.withObject
      "RoutingProfile"
      ( \x ->
          RoutingProfile'
            Prelude.<$> (x Core..:? "InstanceId")
            Prelude.<*> (x Core..:? "RoutingProfileArn")
            Prelude.<*> (x Core..:? "RoutingProfileId")
            Prelude.<*> (x Core..:? "DefaultOutboundQueueId")
            Prelude.<*> (x Core..:? "Name")
            Prelude.<*> ( x Core..:? "MediaConcurrencies"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "Description")
            Prelude.<*> (x Core..:? "Tags" Core..!= Prelude.mempty)
      )

instance Prelude.Hashable RoutingProfile where
  hashWithSalt salt' RoutingProfile' {..} =
    salt' `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` mediaConcurrencies
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` defaultOutboundQueueId
      `Prelude.hashWithSalt` routingProfileId
      `Prelude.hashWithSalt` routingProfileArn
      `Prelude.hashWithSalt` instanceId

instance Prelude.NFData RoutingProfile where
  rnf RoutingProfile' {..} =
    Prelude.rnf instanceId
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf mediaConcurrencies
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf defaultOutboundQueueId
      `Prelude.seq` Prelude.rnf routingProfileId
      `Prelude.seq` Prelude.rnf routingProfileArn

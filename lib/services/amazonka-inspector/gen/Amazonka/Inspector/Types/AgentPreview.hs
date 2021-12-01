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
-- Module      : Amazonka.Inspector.Types.AgentPreview
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Inspector.Types.AgentPreview where

import qualified Amazonka.Core as Core
import Amazonka.Inspector.Types.AgentHealth
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Used as a response element in the PreviewAgents action.
--
-- /See:/ 'newAgentPreview' smart constructor.
data AgentPreview = AgentPreview'
  { -- | The hostname of the EC2 instance on which the Amazon Inspector Agent is
    -- installed.
    hostname :: Prelude.Maybe Prelude.Text,
    -- | The Auto Scaling group for the EC2 instance where the agent is
    -- installed.
    autoScalingGroup :: Prelude.Maybe Prelude.Text,
    -- | The operating system running on the EC2 instance on which the Amazon
    -- Inspector Agent is installed.
    operatingSystem :: Prelude.Maybe Prelude.Text,
    -- | The version of the Amazon Inspector Agent.
    agentVersion :: Prelude.Maybe Prelude.Text,
    -- | The kernel version of the operating system running on the EC2 instance
    -- on which the Amazon Inspector Agent is installed.
    kernelVersion :: Prelude.Maybe Prelude.Text,
    -- | The health status of the Amazon Inspector Agent.
    agentHealth :: Prelude.Maybe AgentHealth,
    -- | The IP address of the EC2 instance on which the Amazon Inspector Agent
    -- is installed.
    ipv4Address :: Prelude.Maybe Prelude.Text,
    -- | The ID of the EC2 instance where the agent is installed.
    agentId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AgentPreview' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'hostname', 'agentPreview_hostname' - The hostname of the EC2 instance on which the Amazon Inspector Agent is
-- installed.
--
-- 'autoScalingGroup', 'agentPreview_autoScalingGroup' - The Auto Scaling group for the EC2 instance where the agent is
-- installed.
--
-- 'operatingSystem', 'agentPreview_operatingSystem' - The operating system running on the EC2 instance on which the Amazon
-- Inspector Agent is installed.
--
-- 'agentVersion', 'agentPreview_agentVersion' - The version of the Amazon Inspector Agent.
--
-- 'kernelVersion', 'agentPreview_kernelVersion' - The kernel version of the operating system running on the EC2 instance
-- on which the Amazon Inspector Agent is installed.
--
-- 'agentHealth', 'agentPreview_agentHealth' - The health status of the Amazon Inspector Agent.
--
-- 'ipv4Address', 'agentPreview_ipv4Address' - The IP address of the EC2 instance on which the Amazon Inspector Agent
-- is installed.
--
-- 'agentId', 'agentPreview_agentId' - The ID of the EC2 instance where the agent is installed.
newAgentPreview ::
  -- | 'agentId'
  Prelude.Text ->
  AgentPreview
newAgentPreview pAgentId_ =
  AgentPreview'
    { hostname = Prelude.Nothing,
      autoScalingGroup = Prelude.Nothing,
      operatingSystem = Prelude.Nothing,
      agentVersion = Prelude.Nothing,
      kernelVersion = Prelude.Nothing,
      agentHealth = Prelude.Nothing,
      ipv4Address = Prelude.Nothing,
      agentId = pAgentId_
    }

-- | The hostname of the EC2 instance on which the Amazon Inspector Agent is
-- installed.
agentPreview_hostname :: Lens.Lens' AgentPreview (Prelude.Maybe Prelude.Text)
agentPreview_hostname = Lens.lens (\AgentPreview' {hostname} -> hostname) (\s@AgentPreview' {} a -> s {hostname = a} :: AgentPreview)

-- | The Auto Scaling group for the EC2 instance where the agent is
-- installed.
agentPreview_autoScalingGroup :: Lens.Lens' AgentPreview (Prelude.Maybe Prelude.Text)
agentPreview_autoScalingGroup = Lens.lens (\AgentPreview' {autoScalingGroup} -> autoScalingGroup) (\s@AgentPreview' {} a -> s {autoScalingGroup = a} :: AgentPreview)

-- | The operating system running on the EC2 instance on which the Amazon
-- Inspector Agent is installed.
agentPreview_operatingSystem :: Lens.Lens' AgentPreview (Prelude.Maybe Prelude.Text)
agentPreview_operatingSystem = Lens.lens (\AgentPreview' {operatingSystem} -> operatingSystem) (\s@AgentPreview' {} a -> s {operatingSystem = a} :: AgentPreview)

-- | The version of the Amazon Inspector Agent.
agentPreview_agentVersion :: Lens.Lens' AgentPreview (Prelude.Maybe Prelude.Text)
agentPreview_agentVersion = Lens.lens (\AgentPreview' {agentVersion} -> agentVersion) (\s@AgentPreview' {} a -> s {agentVersion = a} :: AgentPreview)

-- | The kernel version of the operating system running on the EC2 instance
-- on which the Amazon Inspector Agent is installed.
agentPreview_kernelVersion :: Lens.Lens' AgentPreview (Prelude.Maybe Prelude.Text)
agentPreview_kernelVersion = Lens.lens (\AgentPreview' {kernelVersion} -> kernelVersion) (\s@AgentPreview' {} a -> s {kernelVersion = a} :: AgentPreview)

-- | The health status of the Amazon Inspector Agent.
agentPreview_agentHealth :: Lens.Lens' AgentPreview (Prelude.Maybe AgentHealth)
agentPreview_agentHealth = Lens.lens (\AgentPreview' {agentHealth} -> agentHealth) (\s@AgentPreview' {} a -> s {agentHealth = a} :: AgentPreview)

-- | The IP address of the EC2 instance on which the Amazon Inspector Agent
-- is installed.
agentPreview_ipv4Address :: Lens.Lens' AgentPreview (Prelude.Maybe Prelude.Text)
agentPreview_ipv4Address = Lens.lens (\AgentPreview' {ipv4Address} -> ipv4Address) (\s@AgentPreview' {} a -> s {ipv4Address = a} :: AgentPreview)

-- | The ID of the EC2 instance where the agent is installed.
agentPreview_agentId :: Lens.Lens' AgentPreview Prelude.Text
agentPreview_agentId = Lens.lens (\AgentPreview' {agentId} -> agentId) (\s@AgentPreview' {} a -> s {agentId = a} :: AgentPreview)

instance Core.FromJSON AgentPreview where
  parseJSON =
    Core.withObject
      "AgentPreview"
      ( \x ->
          AgentPreview'
            Prelude.<$> (x Core..:? "hostname")
            Prelude.<*> (x Core..:? "autoScalingGroup")
            Prelude.<*> (x Core..:? "operatingSystem")
            Prelude.<*> (x Core..:? "agentVersion")
            Prelude.<*> (x Core..:? "kernelVersion")
            Prelude.<*> (x Core..:? "agentHealth")
            Prelude.<*> (x Core..:? "ipv4Address")
            Prelude.<*> (x Core..: "agentId")
      )

instance Prelude.Hashable AgentPreview where
  hashWithSalt salt' AgentPreview' {..} =
    salt' `Prelude.hashWithSalt` agentId
      `Prelude.hashWithSalt` ipv4Address
      `Prelude.hashWithSalt` agentHealth
      `Prelude.hashWithSalt` kernelVersion
      `Prelude.hashWithSalt` agentVersion
      `Prelude.hashWithSalt` operatingSystem
      `Prelude.hashWithSalt` autoScalingGroup
      `Prelude.hashWithSalt` hostname

instance Prelude.NFData AgentPreview where
  rnf AgentPreview' {..} =
    Prelude.rnf hostname
      `Prelude.seq` Prelude.rnf agentId
      `Prelude.seq` Prelude.rnf ipv4Address
      `Prelude.seq` Prelude.rnf agentHealth
      `Prelude.seq` Prelude.rnf kernelVersion
      `Prelude.seq` Prelude.rnf agentVersion
      `Prelude.seq` Prelude.rnf operatingSystem
      `Prelude.seq` Prelude.rnf autoScalingGroup

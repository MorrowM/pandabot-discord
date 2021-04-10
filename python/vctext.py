import discord
from discord import Object
import json

with open('bot.json', 'r') as f:
    cfg = json.load(f)

BOT_TOKEN = cfg['botToken']

client = discord.Client()


@client.event
async def on_ready():
    print('Python bot ready.')


@client.event
async def on_voice_state_update(member, before, after):
    for vctext_role in cfg['voiceConfig']['roles']:
        if not (before.channel and before.channel.id in vctext_role['voiceChannels']) and \
                (after.channel and after.channel.id in vctext_role['voiceChannels']):
            await member.add_roles(Object(vctext_role['role']),
                                   reason=f'User joined {vctext_role["name"]}')
            print(
                f'Added the {vctext_role["name"]} vctext role to {member.name}')

            text_channel = vctext_role.get('textChannel')
            if text_channel is not None:
                await client.get_channel(text_channel).send(f'{member.name} joined the channel')

        if (before.channel and before.channel.id in vctext_role['voiceChannels']) and \
                not (after.channel and after.channel.id in vctext_role['voiceChannels']):
            await member.remove_roles(Object(vctext_role['role']),
                                      reason=f'User left {vctext_role["name"]}')
            print(
                f'Removed the {vctext_role["name"]} vctext role from {member.name}')

            text_channel = vctext_role.get('textChannel')
            if text_channel is not None:
                await client.get_channel(text_channel).send(f'{member.name} left the channel')

client.run(BOT_TOKEN)
